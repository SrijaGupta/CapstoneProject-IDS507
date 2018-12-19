select CONCAT(n.FIRST_NAME,' ',n.MIDDLE_NAME,' ',n.LAST_NAME) as National_DB_Name, 
CONCAT(d.First__MI,' ', d.[Middle Name],' ', d.Last_Name) as Delta_Name,n.FULL_ADDRESS,d.St_Address
from Name as N join [Delta Delta2] as D
on (n.FIRST_NAME = d.First__MI and n.LAST_NAME = d.Last_Name and n.MIDDLE_NAME=d.[Middle Name])


select count(*) from Name
select count(*) from [Delta Delta2]
select count(*)
from Name as N join [Delta Delta2] as D
on (n.FIRST_NAME = d.First__MI and n.LAST_NAME = d.Last_Name and n.MIDDLE_NAME=d.[Middle Name])



