/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmLoanInstanceAcmGroupeApprovalDTO;

/**
 * The Interface AcmLoanInstanceGroupe_AssociationService.
 */
public interface AcmLoanInstanceAcmGroupeApprovalService {

	/**
	 * Save.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the acm loan instance groupe association DTO
	 */
	AcmLoanInstanceAcmGroupeApprovalDTO save(
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the acm loan instance groupe association DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmLoanInstanceAcmGroupeApprovalDTO save(Long id,
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO)
			throws ResourcesNotFoundException;;

	/**
	 * Find loan instance grps.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @return the list
	 */
	List<AcmLoanInstanceAcmGroupeApprovalDTO> find(
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO);

	/**
	 * Update all.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTOs the acm loan instance groupe association DT os
	 */
	void updateAll(List<AcmLoanInstanceAcmGroupeApprovalDTO> acmLoanInstanceGroupeAssociationDTOs);

}
