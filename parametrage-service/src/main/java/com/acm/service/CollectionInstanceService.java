/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CollectionInstanceDTO;

/**
 * {@link CollectionInstanceService} interface.
 *
 * @author idridi
 * @since 1.1.9
 */
public interface CollectionInstanceService {

	/**
	 * Find.
	 *
	 * @author idridi
	 * @param collectionInstanceDTO the collection instance DTO
	 * @return the list
	 */
	List<CollectionInstanceDTO> find(CollectionInstanceDTO collectionInstanceDTO);

	/**
	 * Save.
	 *
	 * @author idridi
	 * @param collectionInstanceDTO the collection instance DTO
	 * @return the collection instance DTO
	 */
	CollectionInstanceDTO save(CollectionInstanceDTO collectionInstanceDTO);

	// List<LoanInstanceDTO> updateForWorkflow(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @author idridi
	 * @param id the id
	 * @param collectionInstanceDTO the collection instance DTO
	 * @return the collection instance DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CollectionInstanceDTO save(Long id, CollectionInstanceDTO collectionInstanceDTO)
			throws ResourcesNotFoundException;

}
