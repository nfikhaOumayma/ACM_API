/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.utils.dtos.LoansUdfDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.models.UserDefinedFieldsLinks;

/**
 * This class @{link UDFSettingController}.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/udf-links")
public class UserDefinedFieldsLinksController {

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinkService;

	/**
	 * Create the UserDefinedFieldsLinks.
	 *
	 * @author MoezMhiri
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTO
	 */
	@PostMapping("/create")
	public UserDefinedFieldsLinksDTO create(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		return userDefinedFieldsLinkService.save(userDefinedFieldsLinksDTO);
	}

	/**
	 * Create the {@link UserDefinedFieldsLinksDTO} Used by BATCH.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create-by-batch")
	public UserDefinedFieldsLinksDTO createByBatch(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO)
			throws ResourcesNotFoundException {

		return userDefinedFieldsLinkService.saveByBatch(userDefinedFieldsLinksDTO);
	}

	/**
	 * Create all the UserDefinedFieldsLinks.
	 *
	 * @author MoezMhiri
	 * @param userDefinedFieldsLinksDTOs the list userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTOs
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create-all")
	public List<UserDefinedFieldsLinksDTO> create(
			@RequestBody List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs)
			throws ResourcesNotFoundException {

		return userDefinedFieldsLinkService.saveAll(userDefinedFieldsLinksDTOs);
	}

	/**
	 * Update the UserDefinedFieldsLinks by id.
	 *
	 * @author MoezMhiri
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public UserDefinedFieldsLinksDTO update(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO)
			throws ResourcesNotFoundException {

		return userDefinedFieldsLinkService.save(userDefinedFieldsLinksDTO.getId(),
				userDefinedFieldsLinksDTO);
	}

	/**
	 * Find {@link List} of {@link UserDefinedFieldsLinks} by Requested params.
	 * 
	 * @author MoezMhiri
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<UserDefinedFieldsLinksDTO> find(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		return userDefinedFieldsLinkService.find(userDefinedFieldsLinksDTO);
	}

	/**
	 * Find {@link List} of {@link UDFLinksGroupeFieldsDTO} by given params => list UDF group By
	 * udfGroupID.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the list
	 */
	@PostMapping("/find-udf-groupby")
	public List<UDFLinksGroupeFieldsDTO> findUDFGroupBy(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		return userDefinedFieldsLinkService.findUDFGroupBy(userDefinedFieldsLinksDTO);
	}

	/**
	 * Find loans udf by customer.
	 *
	 * @author Ines Dridi
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the list
	 */
	@PostMapping("/find-udf-loans-bycustomer")
	public List<LoansUdfDTO> findUDFLoansGroupBy(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		return userDefinedFieldsLinkService.findUDFLoansGroupBy(userDefinedFieldsLinksDTO);
	}

	/**
	 * Update udf links by element id.
	 *
	 * @param userDefinedFieldsLinksDTOs the user defined fields links DT os
	 * @param elementId the element id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-udf-links-by-elementId/{elementId}")
	public void updateUdfLinksByElementId(
			@RequestBody List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs,
			@PathVariable("elementId") Long elementId) throws ResourcesNotFoundException {

		userDefinedFieldsLinkService.updateAcmUdfLinksByElementId(userDefinedFieldsLinksDTOs,
				elementId, null);
	}

	/**
	 * Find max index group.
	 *
	 * @param elementId the element id
	 * @param category the category
	 * @return the integer
	 */
	@GetMapping("/find-max-index-group/{elementId}/{category}")
	public Integer findMaxIndexGroup(@PathVariable("elementId") Long elementId,
			@PathVariable("category") String category) {

		return userDefinedFieldsLinkService.findMaxIndexGroup(elementId, category);
	}
}
