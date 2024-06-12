/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.CustomerAbacusService;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.LoanDetailsInfoResponseDTO;
import com.acm.utils.dtos.ScheduleDTO;

/**
 * This class @{link LoadDataCustomerController}.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataCustomerController {

	/** The customer abacus service. */
	@Autowired
	private CustomerAbacusService customerAbacusService;

	/**
	 * Find customer by ID.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @return the customer DTO
	 */
	@GetMapping("/customer/{idCustomer}")
	public CustomerDTO find(@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.find(idCustomer);
	}

	/**
	 * Find customers.
	 *
	 * @param idCustomerExtern the id customer extern
	 * @return the list
	 */
	@GetMapping("/get-customers-for-batch/{idCustomerExtern}")
	public List<CustomerDTO> findCustomers(
			@PathVariable("idCustomerExtern") Long idCustomerExtern) {

		return customerAbacusService.findCustomers(idCustomerExtern);
	}

	/**
	 * load all customer.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	@GetMapping("/customer/all")
	public List<CustomerDTO> load() {

		return customerAbacusService.find();
	}

	/**
	 * Find customer by idLoan.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id Loan
	 * @return the customer DTO
	 */
	@GetMapping("/customer-loan/{idLoan}")
	public CustomerDTO findByLoan(@PathVariable("idLoan") Long idLoan) {

		return customerAbacusService.findByLoan(idLoan);
	}

	/**
	 * Find customer previous account by loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/customer-account/{idLoan}")
	public List<CustomerAccountDTO> findAccountByLoan(@PathVariable("idLoan") Long idLoan) {

		return customerAbacusService.findAccountByLoan(idLoan);
	}

	/**
	 * Find customer previous account by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/account-by-customer/{idCustomer}")
	public List<CustomerAccountDTO> findAccountByCustomer(
			@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findAccountByCustomer(idCustomer);
	}

	/**
	 * Find customer account schedule by loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/customer-account-schedule/{idLoan}")
	public List<ScheduleDTO> findCustomerAccountScheduleByLoan(
			@PathVariable("idLoan") Long idLoan) {

		return customerAbacusService.findCustomerAccountScheduleByLoan(idLoan);
	}

	/**
	 * Find members group by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/find-members-group/{idCustomer}")
	public List<CustomerMemberDTO> findMembersGroupByCustomer(
			@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findMembersGroupByCustomer(idCustomer);
	}

	/**
	 * Find members organisation by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/find-members-organisation/{idCustomer}")
	public List<CustomerMemberDTO> findMembersOrganisationByCustomer(
			@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findMembersOrganisationByCustomer(idCustomer);
	}

	/**
	 * Find relationship by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/find-relationship/{idCustomer}")
	public List<CustomerMemberDTO> findRelationshipByCustomer(
			@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findRelationshipByCustomer(idCustomer);
	}

	/**
	 * Find customer active account.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @param idProduct the id product
	 * @return the long
	 */
	@GetMapping("/find-customer-active-account/{idCustomer}/{idProduct}")
	public Long findCustomerActiveAccount(@PathVariable("idCustomer") Long idCustomer,
			@PathVariable("idProduct") Long idProduct) {

		return customerAbacusService.findCustomerActiveAccount(idCustomer, idProduct);
	}

	/**
	 * Find arrears by idCustomer.
	 * 
	 * @author Salmen Fatnassi
	 * @param idCustomer the id customer
	 * @return the arrears DTO
	 */
	@GetMapping("/customer-arrears/{idCustomer}")
	public ArrearsDTO findArrearsByCustomer(@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findArrearsByCustomer(idCustomer);
	}

	/**
	 * Find all active accounts for customer.
	 *
	 * @author MoezMhiri
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/find-all-active-accounts-for-customer/{idCustomer}")
	public List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(
			@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findAllActiveAccountsForCustomer(idCustomer);
	}

	/**
	 * Find customer paid account.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @param idProduct the id product
	 * @return the double
	 */
	@GetMapping("/find-customer-paid-account/{idCustomer}/{idProduct}")
	public Double findCustomerPaidAccount(@PathVariable("idCustomer") Long idCustomer,
			@PathVariable("idProduct") Long idProduct) {

		return customerAbacusService.findCustomerPaidAccount(idCustomer, idProduct);
	}

	/**
	 * Find customer paid accounts.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/find-customer-paid-accounts/{idCustomer}")
	public List<Double> findCustomerPaidAccounts(@PathVariable("idCustomer") Long idCustomer) {

		return customerAbacusService.findCustomerPaidAccounts(idCustomer);
	}

	/**
	 * Load check paiment for given customer by customerNumber => return Map grouped by LoanID.
	 *
	 * @author HaythemBenizid
	 * @param customerNumber the customer number
	 * @return the map
	 */
	@GetMapping("/customer-check-paiment/{customerNumber}")
	public Map<Long, List<ScheduleDTO>> loadCheckPaiment(
			@PathVariable("customerNumber") String customerNumber) {

		return customerAbacusService.loadCheckPaiment(customerNumber);
	}

	/**
	 * Find loan details informations by loan.
	 *
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/find-loan-Details-informations/{idLoan}")
	public List<LoanDetailsInfoResponseDTO> findLoanDetailsInformationsByLoan(
			@PathVariable("idLoan") Long idLoan) {

		return customerAbacusService.findLoanDetailsInformationsByLoan(idLoan);
	}

}
