/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.LoansUdfDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * {@link UserDefinedFieldsLinksService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface UserDefinedFieldsLinksService {

	/**
	 * Find {@link UserDefinedFieldsLinksDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the userDefinedFieldsLinks DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldsLinksDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link UserDefinedFieldsLinksDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the list
	 */
	List<UserDefinedFieldsLinksDTO> find(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * Find {@link List} of {@link UDFLinksGroupeFieldsDTO} by given params => list UDF group By
	 * udfGroupID.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the list
	 */
	List<UDFLinksGroupeFieldsDTO> findUDFGroupBy(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * The method used for saving the given {@link UserDefinedFieldsLinksDTO}.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTO
	 */
	UserDefinedFieldsLinksDTO save(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * The method used for saving the given {@link UserDefinedFieldsLinksDTO} use Only by Batch.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldsLinksDTO saveByBatch(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO)
			throws ResourcesNotFoundException;

	/**
	 * The method used for updating the given {@link UserDefinedFieldsLinksDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param userDefinedFieldsLinksDTO the userDefinedFieldsLinks DTO
	 * @return the userDefinedFieldsLinks DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldsLinksDTO save(Long id, UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save all.
	 * 
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTOs the user defined fields links DT os
	 * @return the list
	 */
	List<UserDefinedFieldsLinksDTO> saveAll(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs);

	/**
	 * Delete {@link UserDefinedFieldsLinksDTO} by ByCustomer ID.
	 * 
	 * @author MoezMhiri
	 * @param customerId customerId
	 */
	void deleteAllByCustomer(Long customerId);

	/**
	 * Delete {@link UserDefinedFieldsLinksDTO} by Loan ID.
	 * 
	 * @author MoezMhiri
	 * @param loanId loanId
	 */
	void deleteAllByLoan(Long loanId);

	/**
	 * Delete {@link UserDefinedFieldsLinksDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 */
	void delete(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * Find UDF loans group by.
	 *
	 * @author Ines Dridi
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the list
	 */
	List<LoansUdfDTO> findUDFLoansGroupBy(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * Delete {@link UserDefinedFieldsLinksDTO} by customer id and id abacus UDF link is null and
	 * surveys id is null.
	 * 
	 * @author YesserSomai
	 * @param customerId the customer id
	 */
	void deleteByCustomerIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(Long customerId);

	/**
	 * Delete {@link UserDefinedFieldsLinksDTO} by Loan id and id abacus UDF link is null and
	 * surveys id is null.
	 *
	 * @author YesserSomai
	 * @param loanId the loan id
	 */
	void deleteByLoanIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(Long loanId);

	/**
	 * Save for topup.
	 *
	 * @author mlamloum
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the user defined fields links DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldsLinksDTO saveForTopup(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO)
			throws ResourcesNotFoundException;

	/**
	 * Update acm udf links by element id.
	 *
	 * @param userDefinedFieldsLinksDTOs the user defined fields links DT os
	 * @param elementId the element id
	 * @param category the category
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateAcmUdfLinksByElementId(List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs,
			Long elementId, String category) throws ResourcesNotFoundException;

	/**
	 * Update abacus udf links that are related to Abacus udf fields.
	 *
	 * @param elementId the element id
	 * @param category the category
	 * @param idExtern the id extern
	 * @param object the object
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateAbacusUdfLinksByElementId(Long elementId, String category, Long idExtern,
			Object object) throws ResourcesNotFoundException;

	/**
	 * Delete all by category.
	 *
	 * @param elementId the element id
	 * @param category the category
	 */
	void deleteAllByCategory(Long elementId, String category);

	/**
	 * Find max index group.
	 *
	 * @param elementId the element id
	 * @param category the category
	 * @return the integer
	 */
	Integer findMaxIndexGroup(Long elementId, String category);
}
