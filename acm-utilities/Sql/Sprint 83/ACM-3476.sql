/** author : Meher Khemissi */ 
--get  the id of nationalty , place of issue and family situation and family situatio
--And then put them in the request delete

select f.CODE_FIELD, *  from ACM_IHM_VALIDATOR_FIELD v 
join ACM_IHM_FIELD f on f.ID = v.ID_ACM_IHM_FIELD
where ID_ACM_IHM_VALIDATOR=1

delete from ACM_IHM_VALIDATOR_FIELD where ID_ACM_IHM_FIELD in (49,53,41,13)