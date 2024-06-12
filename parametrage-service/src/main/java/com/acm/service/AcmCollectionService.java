/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.pagination.CollectionPaginationDTO;
import com.acm.utils.models.AcmCollection;

/**
 * The Interface AcmCollectionService.
 */
public interface AcmCollectionService {

	/**
	 * Find.
	 *
	 * @param acmCollectionDTO the acm collection DTO
	 * @param checkOnOwnerIsConnectedUser the check on owner is connected user
	 * @return the list
	 */
	List<AcmCollectionDTO> find(AcmCollectionDTO acmCollectionDTO,
			Boolean checkOnOwnerIsConnectedUser);

	/**
	 * Find.
	 *
	 * @return the list
	 */
	List<AcmCollectionDTO> find();

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param collectionPaginationDTO the collection pagination DTO
	 * @return the collection pagination DTO
	 */
	CollectionPaginationDTO find(CollectionPaginationDTO collectionPaginationDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmCollectionDTO save(Long id, AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmCollectionDTO save(AcmCollectionDTO acmCollectionDTO) throws ResourcesNotFoundException;

	/**
	 * Save all.
	 *
	 * @param acmCollections the acm collections
	 * @param processWF the process WF
	 */
	void saveAll(List<AcmCollection> acmCollections, String processWF);

	/**
	 * Close collections.
	 * 
	 * @author idridi
	 * @return the int
	 */
	int closeCollections();

	/**
	 * Close collection tasks.
	 */
	void closeCollectionTasks();

	/**
	 * Load filter branch.
	 * 
	 * @author idridi
	 * @param acmCollection the acm collection
	 * @return the list
	 */
	List<AcmCollectionDTO> loadFilterBranch(AcmCollectionDTO acmCollection);

	/**
	 * Load filter status.
	 * 
	 * @author idridi
	 * @param acmCollection the acm collection
	 * @return the list
	 */
	List<AcmCollectionDTO> loadFilterStatus(AcmCollectionDTO acmCollection);

	/**
	 * Load filter product.
	 * 
	 * @author idridi
	 * @param acmCollection the acm collection
	 * @return the list
	 */
	List<AcmCollectionDTO> loadFilterProduct(AcmCollectionDTO acmCollection);

	/**
	 * Complete action.
	 *
	 * @param acmCollection the acm collection
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	AcmCollectionDTO completeAction(AcmCollectionDTO acmCollection)
			throws ResourcesNotFoundException, ApiAbacusException;

	/**
	 * Assign collection.
	 *
	 * @author idridi
	 * @param acmCollection the acm collection
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ParametrageException the parametrage exception
	 */
	AcmCollectionDTO assignCollection(AcmCollectionDTO acmCollection)
			throws ResourcesNotFoundException, ParametrageException;

	/**
	 * Evaluer expression.
	 *
	 * @param expression the expression
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the string
	 */
	String evaluerExpression(String expression, AcmCollectionDTO acmCollectionDTO);

}
