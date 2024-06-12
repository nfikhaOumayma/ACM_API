update ACM_CUSTOMER set DATE_OF_BIRTH_HIJRI = FORMAT(DATE_OF_BIRTH,'yyyy-MM-dd','ar')
where DATE_OF_BIRTH is not null