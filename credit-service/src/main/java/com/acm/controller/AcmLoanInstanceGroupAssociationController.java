/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmLoanInstanceAcmGroupeApprovalRepository;
import com.acm.service.AcmLoanInstanceAcmGroupeApprovalService;
import com.acm.utils.dtos.AcmLoanInstanceAcmGroupeApprovalDTO;
import com.acm.utils.models.AcmLoanInstanceAcmGroupeApproval;

/**
 * The Class AcmLoanInstanceGroup_Association.
 */
@RestController
@RequestMapping("/acm-loanInstance-group-association")
public class AcmLoanInstanceGroupAssociationController {

	/** The loan inst group assoc. */
	@Autowired
	private AcmLoanInstanceAcmGroupeApprovalService acmLoanInstanceGroupeAssociationService;
	
	/** The loan ins repo. */
	@Autowired
	private AcmLoanInstanceAcmGroupeApprovalRepository acmLoanInstanceGroupeAssociationRepository;

	/**
	 * Creates the.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the acm loan instance groupe association DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public AcmLoanInstanceAcmGroupeApprovalDTO create(
			@RequestBody AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO)
			throws ResourcesNotFoundException {

		return acmLoanInstanceGroupeAssociationService.save(acmLoanInstanceGroupeAssociationDTO);
	}

	/**
	 * Find loan instance groups.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the list
	 */
	@PostMapping("/find-loanInstanceGroups")
	public List<AcmLoanInstanceAcmGroupeApprovalDTO> findLoanInstanceGroups(
			@RequestBody AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO) {

		return acmLoanInstanceGroupeAssociationService.find(acmLoanInstanceGroupeAssociationDTO);
	}
	

	/**
	 * Find by validation.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/findByValidation")
	public List<AcmLoanInstanceAcmGroupeApproval> findByValidation(
			@RequestBody Boolean acmLoanInstanceGroupeAssociationDTO)
			throws ResourcesNotFoundException {

		return acmLoanInstanceGroupeAssociationRepository.findByValidation(acmLoanInstanceGroupeAssociationDTO);
	}

	/**
	 * Update.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the acm loan instance groupe association DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update")
	public AcmLoanInstanceAcmGroupeApprovalDTO update(
			@RequestBody AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO)
			throws ResourcesNotFoundException {

		return acmLoanInstanceGroupeAssociationService.save(acmLoanInstanceGroupeAssociationDTO.getId(),
				acmLoanInstanceGroupeAssociationDTO);
	}

}
