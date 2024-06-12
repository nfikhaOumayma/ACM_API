package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.CollectionNote;

@Repository
public interface CollectionNoteRepository
		extends JpaRepository<CollectionNote, Long>, QuerydslPredicateExecutor<CollectionNote>,
		CrudRepository<CollectionNote, Long>, PagingAndSortingRepository<CollectionNote, Long> {

}
