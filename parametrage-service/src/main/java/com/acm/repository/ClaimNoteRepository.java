/*
 * 
 */
package com.acm.repository;

import org.springframework.stereotype.Repository;

import com.acm.utils.models.ClaimNote;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;


/**
 * The Interface ClaimNoteRepository.
 */
@Repository
public interface ClaimNoteRepository extends JpaRepository<ClaimNote, Long>, QuerydslPredicateExecutor<ClaimNote>,
CrudRepository<ClaimNote, Long>, PagingAndSortingRepository<ClaimNote, Long> {
}
