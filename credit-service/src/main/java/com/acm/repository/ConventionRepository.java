package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Convention;

/**
 * The Interface ConventionRepository.
 */
@Repository
public interface ConventionRepository extends JpaRepository<Convention, Long> {

	/**
	 * Find by id supplier.
	 *
	 * @param idSupplier the id supplier
	 * @return the list
	 */
	@Query("SELECT convention FROM Convention convention WHERE convention.supplier.id = :idSupplier")
	List<Convention> findByIdSupplier(@Param("idSupplier") Long idSupplier);

}
