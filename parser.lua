local lpeg = require "lpeg"

local Ct,V,S,C,R,P = lpeg.Ct, lpeg.V, lpeg.S, lpeg.C, lpeg.R, lpeg.P

local function sanitize (str)

    if type(str) == 'table' then
        return str
    end

    return (string.gsub(str,"[ \n\t]",""))
end

local space = S(" \n\t")^0

local primitive_types = P("int") * space +
                        P("bool") * space +
                        P("unit") * space
-- values
local n = C( P("-")^-1 * R("09")^1 * space) / function (x) return 'N(' .. x .. ')' end
local b = C( P"true" + P"false" * space) / function (x) return 'B(' .. x .. ')' end
local skip = C( P "skip" * space) / function (x) return 'Skip(' .. x .. ')' end


local op =  C( S"+-*<>" * space)
local unop = C( ( P"!" + P"ref") * space )

local longops = C( ( P"==" + P"!=" + P"<=" + P">=" + P":=" ) * space)

-- variables
local Expr = V "Expr"
local Operand = V "Operand"
local Cond = Ct( V "Cond" ) / function (ct) ct.name = 'If'; return ct end
local Oper =  Ct( V("Oper") ) / function (ct) ct.name = 'Op'; return ct end
local Function = Ct( V "Function" ) / function (ct) ct.name = 'Fn'; return ct end
local Loop = Ct( V "Loop" ) / function (ct) ct.name = 'While'; return ct end
local Value = V "Value" / sanitize
local Type = C(lpeg.V "Type")
local Types = V "Types"
local Op = V "Op" / sanitize
local Unoper = Ct( V "Unoper" ) / function (ct) ct.name = 'Unop'; return ct end
local VarLet = Ct( lpeg.V "VarLet" ) / function (ct) ct.name = 'Let'; return ct end
local VarLetRec = Ct( lpeg.V "VarLetRec") / function (ct) ct.name = 'LetRec'; return ct end
local SeqExpr = Ct(lpeg.V "SeqExpr") / function (ct) ct.name = 'Seq'; return ct end
local App = Ct( V "App" ) / function (ct) ct.name = 'App'; return ct end
local open = P "(" * space
local close = P ")" * space

-- keywords
local If = P "if" * space
local Then = P "then" * space
local Else = P "else" * space
local While = P "while" * space
local Do = P "do" * space
local Let = P "let" * space
local LetRec = P "let rec" * space
local In = P "in" * space
local End = P "end" * space
local Equal = P "=" * space
local Seq = P ";" * space

local Fn = P "fn" * space
local Colon = P ":" * space
local Arrow = P "->" * space
local FArrow = P "=>" * space

local Keywords = If + Then + Else + While + Do + Let +
                 LetRec + Fn + In +
                 End + skip + b + lpeg.P "ref"


local varname = C(
                    (
                        ( R"AZ" + R"az" ) * ( R("09") +
                        ( R"AZ" + R"az" ) )^0 - Keywords) * space
                    ) / function (x) return 'X("' .. sanitize(x) .. '")' end

l3 = P {

    SeqExpr,
    SeqExpr = Expr * (Seq * Expr)^0   ,
    Expr = ( App + Cond + Loop + VarLet + VarLetRec + Oper ) + (open * SeqExpr * close),

    Oper = ( ( Operand * (Op * Operand)^0 ) + Unoper )^0  ,
    Operand = Value + (open * Oper * close),
    Op = op + longops ,
    Unoper = unop * Expr ,

    Cond = If * Expr * Then * SeqExpr * Else * SeqExpr * End ,
    Loop = While * Expr * Do * SeqExpr * End,

    Value = varname + n + b + skip + Function,

    Type = ( Types * (Arrow * Types)^0 )^0 ,
    Types = primitive_types + (open * Type * close),

    Function = Fn * varname  * Colon * Type * FArrow * open * SeqExpr * close ,

    VarLet = Let * varname * Colon * Type * Equal * SeqExpr * In * SeqExpr * End  ,
    VarLetRec = LetRec * varname * Colon * Type * Equal * Function * In * SeqExpr * End,

    App = varname * open * Oper * close

} * - 1

----io.input('tipos.l3')
io.input('exemplo.l3')

text = io.read('*all')

--text = io.read()
print(text)
t = l3:match(text)

--setmetatable(t,{ __tostring = pretty.write} )
--print(t)

local function toTree(t)

    if type(t) == 'nil' then
        return ''
    elseif
        type(t) == 'string' then
        return t
    end

    local ops = { ['+'] = 'Sum' , ['-'] = 'Dif' , ['*'] = 'Prod' ,
              [';'] = 'Seq' , ['='] = 'Eq', [':='] = 'Asg', ['<'] = 'Lt' ,
              ['>'] = 'Gt', ['<='] = 'Leq' , ['>='] = 'Geq', ['!='] = 'Neq',
              ['!'] = 'Deref', ['ref'] = 'Ref'
     }


    local ltypes = { int = 'Inteiro()' , bool = 'Booleano()' , unit = 'Unidade()' }

    local result = ''

    local function gambiarra(str)
        return (str:gsub('[X()]',''))
    end

    if t.name == 'Unop' then
        result = ops[sanitize(t[1])] .. '(' .. toTree(t[2]) .. ')'
    end

    if t.name == 'App' then
        result = 'App' .. '(' ..  toTree(t[1]) .. ',' .. toTree(t[2]) .. ')'
    end

    if t.name == 'Let' then
        result = 'Let(' .. gambiarra(toTree(t[1])) .. ',' .. toTree(t[2]) .. ',' .. toTree(t[3]) .. ',' .. toTree(t[4]) .. ')'
    end

    if t.name == 'LetRec' then
        result = 'LetRec(' .. gambiarra(toTree(t[1])) .. ',' .. toTree(t[2]) .. ',' .. toTree(t[3]) .. ',' .. toTree(t[4]) .. ')'
    end

    if t.name == 'Fn' then
        result = 'Fn(' .. gambiarra(toTree(t[1])) .. ',' .. toTree(t[2]) .. ',' .. toTree(t[3]) .. ')'
    end

    if t.name == 'If' then
        result = 'If(' .. toTree(t[1]) .. ',' .. toTree(t[2]) .. ',' .. toTree(t[3]) .. ')'
    end

    if t.name == 'While' then
        result = 'While(' .. toTree(t[1]) .. ',' .. toTree(t[2]) .. ')'
    end

    if t.name == 'Seq' then

        local tail = ''

        if #t == 1 then
            return toTree(t[1])
        end

        for i=1,table.getn(t) do
            if t[i+1] ~= nil then
                result = result .. 'Seq('
                result = result .. toTree(t[i]) .. ','
            else
                result = result .. toTree(t[i]) .. ')'
            end
        end
        result = result
    end

    if t.name == 'Op' then

        local tail = ')'

        if #t == 1 then
            return toTree(t[1])
        end

        for i=1,table.getn(t),2 do

            if t[i+1] ~= nil then
                result = result .. ops[t[i+1]] .. '('
                result = result .. toTree(t[i]) .. ','
            else
                result = result .. toTree(t[i]) .. ')'
                --tail = tail .. ')'
            end

        end
        result = result --.. tail
    end

    return result

end

print(toTree(t))

