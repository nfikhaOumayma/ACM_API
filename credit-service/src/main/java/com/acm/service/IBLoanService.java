/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IBLoanDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanStatutDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;

/**
 * {@link IBLoanService} interface.
 *
 * @author MoezMhiri
 * @since 1.0.3
 */
public interface IBLoanService {

	/**
	 * Find {@link IBLoanDTO} by given ID.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the IBLoanService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IBLoanDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link IBLoanDTO} by given params.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTO the ib loan DTO
	 * @return the list
	 */
	List<IBLoanDTO> find(IBLoanDTO ibLoanDTO);

	/**
	 * Find {@link IBLoanPaginationDTO} by page size & page number & given params
	 * ({@link IBLoanPaginationDTO}).
	 * 
	 * @author MoezMhiri
	 * @param loanIbPaginationDTO the loan ib pagination DTO
	 * @return the list
	 */
	IBLoanPaginationDTO find(IBLoanPaginationDTO loanIbPaginationDTO);

	/**
	 * The method used for saving the given {@link IBLoanDTO}.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the IBLoanService DTO
	 */
	LoanDTO saveAcmLoanInIB(LoanDTO loanDTO);

	/**
	 * The method used for updating the given {@link IBLoanDTO} by ID.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @param ibLoanDTO the ib loan DTO
	 * @return the IBLoanService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IBLoanDTO save(Long id, IBLoanDTO ibLoanDTO) throws ResourcesNotFoundException;

	/**
	 * The method used for assign the given {@link IBLoanDTO}.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTOs the list ib loan DTO
	 * @return the list ib loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	List<IBLoanDTO> assignedAll(List<IBLoanDTO> ibLoanDTOs)
			throws ResourcesNotFoundException, CalculateAgeException, CreditException;

	/**
	 * The method used for loadFilterProduct the given {@link IBLoanDTO}.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTO the ib loan DTO
	 * @return the list ib loan DTO
	 */
	List<IBLoanDTO> loadFilterProduct(IBLoanDTO ibLoanDTO);

	/**
	 * The method used for accept the given {@link IBLoanDTO} by ID.
	 *
	 * @author MoezMhiri
	 * @author HaythemBenizid
	 * @param id the id
	 * @param ibLoanDTO the ib loan DTO
	 * @return the IBLoanService DTO
	 * @throws Exception the exception
	 */
	IBLoanDTO accept(Long id, IBLoanDTO ibLoanDTO) throws Exception;

	/**
	 * Count loans ib by status. ('0') New || ('1') Accepted || ('-1') Rejected.
	 * 
	 * @author MoezMhiri
	 * @return the loan statut DTO
	 */
	LoanStatutDTO count();

	/**
	 * Load filter status .Used in online application Table.
	 * 
	 * @author MoezMhiri
	 * @param ibLoanDTO the ib loan DTO
	 * @return the list
	 */
	List<IBLoanDTO> loadFilterStatus(IBLoanDTO ibLoanDTO);

	/**
	 * Count by statut and branch Ids and Owners and Only Enabled DATA.
	 *
	 * @author HaythemBenizid
	 * @param statut the statut
	 * @param branchIds the branch ids
	 * @param owners the owners
	 * @return the list
	 */
	Long countByStatutAndBranchIDsAndOwners(Integer statut, List<Integer> branchIds,
			List<String> owners);

	/**
	 * Count by product id and enabled Only.
	 *
	 * @author HaythemBenizid
	 * @param productId the product id
	 * @return the long
	 */
	Long countByProductId(Integer productId);

}
