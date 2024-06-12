/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SimahClassDetails;
import com.acm.utils.models.SimahClassType;

/**
 * The Interface SimahClassDetailsRepository.
 */
@Repository
public interface SimahClassDetailsRepository extends JpaRepository<SimahClassDetails, Integer> {

	/**
	 * Find by simah class type.
	 *
	 * @param simahClassType the simah class type
	 * @return the list
	 */
	List<SimahClassDetails> findBySimahClassType(SimahClassType simahClassType);

}
