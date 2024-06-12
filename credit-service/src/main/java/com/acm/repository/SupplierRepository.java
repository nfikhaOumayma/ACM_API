package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Supplier;

/**
 * The Interface SupplierRepository.
 */
@Repository
public interface SupplierRepository
		extends JpaRepository<Supplier, Long>, QuerydslPredicateExecutor<Supplier>,
		CrudRepository<Supplier, Long>, PagingAndSortingRepository<Supplier, Long> {
	
	/**
	 * Find by register number.
	 *
	 * @param registerNumber the register number
	 * @return the list
	 */
	List<Supplier> findByRegisterNumber(String registerNumber);
	
	/**
	 * Find by identity.
	 *
	 * @param identity the identity
	 * @return the list
	 */
	List<Supplier> findByIdentity(String identity);
}
