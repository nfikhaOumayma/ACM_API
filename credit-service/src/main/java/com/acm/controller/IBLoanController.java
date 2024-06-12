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

import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.IBLoanService;
import com.acm.utils.dtos.IBLoanDTO;
import com.acm.utils.dtos.LoanStatutDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;

/**
 * This class @{link IBLoanController} used to control all the loan requests Form IB.
 *
 * @author MoezMhiri
 * @since 1.0.3
 */
@RestController
@RequestMapping("/loans-ib")
public class IBLoanController {

	/** The loan ib service. */
	@Autowired
	private IBLoanService loanIbService;

	/**
	 * Find by id.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the loan ib DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public IBLoanDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return loanIbService.find(id);
	}

	/**
	 * Find {@link List} of {@link IBLoanDTO} by Requested params.
	 * 
	 * @author MoezMhiri
	 * @param ibLoanDTO the loan ib DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IBLoanDTO> find(@RequestBody IBLoanDTO ibLoanDTO) {

		return loanIbService.find(ibLoanDTO);
	}

	/**
	 * Creates the IBLoanDTO by new value.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTO the loan ib DTO
	 * @return the loan ib DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public IBLoanDTO create(@RequestBody IBLoanDTO ibLoanDTO) throws ResourcesNotFoundException {

		return null;
	}

	/**
	 * Update the parameter by id.
	 * 
	 * @author MoezMhiri
	 * @param ibLoanDTO the loan ib DTO
	 * @return the loan ib DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public IBLoanDTO update(@RequestBody IBLoanDTO ibLoanDTO) throws ResourcesNotFoundException {

		return loanIbService.save(ibLoanDTO.getId(), ibLoanDTO);
	}

	/**
	 * Assigned.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTOs the list ib loan DTO
	 * @return the ib loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	@PutMapping("/assigned-all")
	public List<IBLoanDTO> assignedAll(@RequestBody List<IBLoanDTO> ibLoanDTOs)
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		return loanIbService.assignedAll(ibLoanDTOs);
	}

	/**
	 * Find pagination.
	 * 
	 * @author MoezMhiri
	 * @param loanIbPaginationDTO the loan ib pagination DTO
	 * @return the loan ib pagination DTO
	 */
	@PostMapping("/find-ib-pagination")
	public IBLoanPaginationDTO findIbPagination(
			@RequestBody IBLoanPaginationDTO loanIbPaginationDTO) {

		return loanIbService.find(loanIbPaginationDTO);
	}

	/**
	 * Load filter product.Used in assign loan ib.
	 * 
	 * @author MoezMhiri
	 * @param loanIbDTO the loanIb DTO
	 * @return the list
	 */
	@PostMapping("/load-filter-product-ib")
	public List<IBLoanDTO> loadFilterProduct(@RequestBody IBLoanDTO loanIbDTO) {

		return loanIbService.loadFilterProduct(loanIbDTO);
	}

	/**
	 * Accept the parameter by id.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTO the loan ib DTO
	 * @return the loan ib DTO
	 * @throws Exception the exception
	 */
	@PutMapping("/accept")
	public IBLoanDTO accept(@RequestBody IBLoanDTO ibLoanDTO) throws Exception {

		return loanIbService.accept(ibLoanDTO.getId(), ibLoanDTO);
	}

	/**
	 * Count loans ib by status. ('0') New || ('1') Accepted || ('-1') Rejected.
	 * 
	 * @author MoezMhiri
	 * @return the loan statut DTO
	 */
	@GetMapping("/count")
	public LoanStatutDTO count() {

		return loanIbService.count();
	}
}
