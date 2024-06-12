/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmCollectionService;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.pagination.CollectionPaginationDTO;
import com.acm.utils.models.AcmCollection;

/**
 * The {@link GroupeController} class used to control all the User request .
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RestController
@RequestMapping("/collection")
public class AcmCollectionController {

	/** The groupe service. */
	@Autowired
	private AcmCollectionService collectionService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmCollectionDTO> find(@RequestBody AcmCollectionDTO acmCollectionDTO) {

		// (parameter 'checkOnOwnerIsConnectedUser' = TRUE) get all collections without check on
		// connectedUser = collection owner
		return collectionService.find(acmCollectionDTO, Boolean.TRUE);
	}

	/**
	 * Find all.
	 * 
	 * @author idridi
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<AcmCollectionDTO> findAll() {

		return collectionService.find();
	}

	/**
	 * Find pagination.
	 * 
	 * @author idridi
	 * @param collectionPaginationDTO the collection pagination DTO
	 * @return the collection pagination DTO
	 */
	@PostMapping("/find-collection-pagination")
	public CollectionPaginationDTO findPagination(
			@RequestBody CollectionPaginationDTO collectionPaginationDTO) {

		return collectionService.find(collectionPaginationDTO);
	}

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public AcmCollectionDTO create(@RequestBody AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException {

		return collectionService.save(acmCollectionDTO);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmCollectionDTO update(@RequestBody AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException {

		return collectionService.save(acmCollectionDTO.getId(), acmCollectionDTO);
	}

	/**
	 * Load filter product.
	 * 
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-product")
	public List<AcmCollectionDTO> loadFilterProduct(
			@RequestBody AcmCollectionDTO acmCollectionDTO) {

		return collectionService.loadFilterProduct(acmCollectionDTO);
	}

	/**
	 * Load filter branch.
	 * 
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-branch")
	public List<AcmCollectionDTO> loadFilterBranch(@RequestBody AcmCollectionDTO acmCollectionDTO) {

		return collectionService.loadFilterBranch(acmCollectionDTO);
	}

	/**
	 * Load filter Status.
	 * 
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-status")
	public List<AcmCollectionDTO> loadFilterStatus(@RequestBody AcmCollectionDTO acmCollectionDTO) {

		return collectionService.loadFilterStatus(acmCollectionDTO);
	}

	/**
	 * Complete action.
	 *
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/complete-action")
	public AcmCollectionDTO completeAction(@RequestBody AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException, ApiAbacusException {

		return collectionService.completeAction(acmCollectionDTO);

	}

	/**
	 * Assign collection.
	 *
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ParametrageException the parametrage exception
	 */
	@PutMapping("/assign-collection")
	public AcmCollectionDTO assignCollection(@RequestBody AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException, ParametrageException {

		return collectionService.assignCollection(acmCollectionDTO);
	}

	/**
	 * Save all.
	 *
	 * @param process the process
	 * @param acmCollectionDTOs the acm collection DT os
	 */
	@PostMapping("/save-all/{process}")
	public void saveAll(@PathVariable("process") String process,
			@RequestBody List<AcmCollectionDTO> acmCollectionDTOs) {

		List<AcmCollection> acmCollections = new ArrayList<>();
		acmCollectionDTOs.forEach(acmCollectionDTO -> acmCollections
				.add(mapper.map(acmCollectionDTO, AcmCollection.class)));

		collectionService.saveAll(acmCollections, process);
	}

}
