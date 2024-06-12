/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.ItemInstance;

/**
 * The Interface GenericWorkflowObjectRepository.
 */
@Repository
public interface ItemInstanceRepository extends JpaRepository<ItemInstance, Long> {

	/**
	 * Find by enabled.
	 *
	 * @param b the b
	 * @return the list
	 */
	List<ItemInstance> findByEnabled(boolean b);

}
