/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;

/**
 * This class @{link CustomerLinksRelationshipController}.
 *
 * @author MoezMhiri
 * @since 1.0.7
 */
@RestController
@RequestMapping("/customer-link-relationship")
public class CustomerLinksRelationshipController {

	/** The customer links relationship service. */
	@Autowired
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/**
	 * Find {@link List} of {@link CustomerLinksRelationshipDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param customerLinksRelationshipDTO the customerLinksRelationship DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<CustomerLinksRelationshipDTO> find(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		return customerLinksRelationshipService.find(customerLinksRelationshipDTO);
	}

	/**
	 * Find all members by customer members id.
	 *
	 * @author Salmen Fatnassi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	@PostMapping("/find-members")
	public List<CustomerLinksRelationshipDTO> findAllMembersByCustomerMembersId(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		return customerLinksRelationshipService
				.findAllMembersByCustomerMembersId(customerLinksRelationshipDTO);
	}

	/**
	 * Find {@link List} of {@link CustomerLinksRelationshipDTO} by Requested params only for active
	 * guarantor.
	 *
	 * @author MoezMhiri
	 * @param customerLinksRelationshipDTO the customerLinksRelationship DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/customer-active-guarantor")
	public List<CustomerLinksRelationshipDTO> findAllActiveGuarantor(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		return customerLinksRelationshipService
				.findAllActiveGuarantors(customerLinksRelationshipDTO);
	}

	/**
	 * Find all loan guarantors.
	 * 
	 * @author ManelLamloum
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	@PostMapping("/find-all-loan-guarantors")
	public List<CustomerLinksRelationshipDTO> findAllLoanGuarantors(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		return customerLinksRelationshipService.findAllLoanGuarantors(customerLinksRelationshipDTO);
	}

	/**
	 * Find guarantees.
	 * 
	 * @author ManelLamloum
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-guarantees")
	public List<CustomerLinksRelationshipDTO> findGuarantees(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		return customerLinksRelationshipService.findGuarantees(customerLinksRelationshipDTO);
	}

	/**
	 * Delete guarantor.
	 *
	 * @author Yesser Somai
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/delete-guarantor")
	public void cancelGuarantorLoan(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		customerLinksRelationshipService.cancelGuarantorLoan(customerLinksRelationshipDTO);
	}

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the customer links relationship DTO
	 */
	@PostMapping("/create")
	public CustomerLinksRelationshipDTO create(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		return customerLinksRelationshipService.save(customerLinksRelationshipDTO);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the customer links relationship DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public CustomerLinksRelationshipDTO update(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		return customerLinksRelationshipService.save(customerLinksRelationshipDTO.getId(),
				customerLinksRelationshipDTO);
	}

	/**
	 * Find guarantors from IB and save in acm.
	 *
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-guarantors-from-ib-and-save-in-acm")
	public List<CustomerLinksRelationshipDTO> findGuarantorsFromIBAndSaveInAcm(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		return customerLinksRelationshipService
				.findGuarantorsFromIBAndSaveInAcm(customerLinksRelationshipDTO);
	}
}
