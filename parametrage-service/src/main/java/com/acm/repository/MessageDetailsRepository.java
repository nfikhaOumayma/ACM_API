package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.MessageDetails;

@Repository
public interface MessageDetailsRepository extends JpaRepository<MessageDetails, Long>,
		QuerydslPredicateExecutor<MessageDetails>, CrudRepository<MessageDetails, Long> {

}
