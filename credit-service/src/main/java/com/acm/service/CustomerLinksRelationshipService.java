/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;

/**
 * {@link CustomerLinksRelationshipService} interface.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public interface CustomerLinksRelationshipService {

	/**
	 * Find {@link List} of {@link CustomerDTO} by given params.
	 *
	 * @author YesserSomai
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	List<CustomerLinksRelationshipDTO> find(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * The method used for saving the given {@link CustomerLinksRelationshipDTO}.
	 *
	 * @author HaythemBenizid
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the CustomerService DTO
	 */
	CustomerLinksRelationshipDTO save(CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Delete {@link customerLinksRelationshipDTO} by given params.
	 * 
	 * @author MoezMhiri
	 * @param customerId customerId
	 * @param category category
	 */
	void deleteByIdCustomerAndCategory(Long customerId, String category);

	/**
	 * Delete {@link customerLinksRelationshipDTO} by given params Loan Id and Category.
	 * 
	 * @author YesserSomai
	 * @param idLoan customerId
	 * @param category customerId
	 */
	void deleteByIdLoanAndCategory(Long idLoan, String category);

	/**
	 * The method used for updating the given {@link CustomerLinksRelationshipDTO} by ID.
	 * 
	 * @author MoezMhiri
	 * @param id the id
	 * @param customerLinksRelationshipDTO the customerLinksRelationship dto
	 * @return the CustomerLinksRelationship DTO
	 * @throws ResourcesNotFoundException the ResourcesNotFoundException
	 */
	CustomerLinksRelationshipDTO save(Long id,
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find all members by customer members id.
	 * 
	 * @author Salmen Fatnassi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	List<CustomerLinksRelationshipDTO> findAllMembersByCustomerMembersId(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Find all active guarantors.
	 *
	 * @author MoezMhiri
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<CustomerLinksRelationshipDTO> findAllActiveGuarantors(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException;

	/**
	 * Cancel relation GUARANTOR - LOAN.
	 *
	 * @author HaythemBenizid
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void cancelGuarantorLoan(CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find all loan guarantors.
	 * 
	 * @author ManelLamloum
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	List<CustomerLinksRelationshipDTO> findAllLoanGuarantors(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Find guarantees.
	 *
	 * @author ManelLamloum
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<CustomerLinksRelationshipDTO> findGuarantees(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find guarantors from IB and save in acm.
	 *
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	List<CustomerLinksRelationshipDTO> findGuarantorsFromIBAndSaveInAcm(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

}
