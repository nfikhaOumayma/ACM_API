/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;
import java.util.Map;

import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.LoanDetailsInfoResponseDTO;
import com.acm.utils.dtos.ScheduleDTO;

/**
 * {@link CustomerAbacusService} interface.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public interface CustomerAbacusService {

	/**
	 * load all Customer from (ABACUS).
	 * 
	 * @author MOEZ
	 * @return the customer DTO
	 */
	List<CustomerDTO> find();

	/**
	 * Find Customer by ID Customer (ABACUS).
	 * 
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @return the customer DTO
	 */
	CustomerDTO find(Long idCustomer);

	/**
	 * Find customers.
	 * 
	 * @author idridi
	 * @param idCustomerExtern the id customer extern
	 * @return the list
	 */
	List<CustomerDTO> findCustomers(Long idCustomerExtern);

	/**
	 * Find Customer by loan .
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the customer DTO
	 */
	CustomerDTO findByLoan(Long idLoan);

	/**
	 * Find old Customer account by loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	List<CustomerAccountDTO> findAccountByLoan(Long idLoan);

	/**
	 * Find old Customer account by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<CustomerAccountDTO> findAccountByCustomer(Long idCustomer);

	/**
	 * Find customer account schedule by loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	List<ScheduleDTO> findCustomerAccountScheduleByLoan(Long idLoan);

	/**
	 * Find members group by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<CustomerMemberDTO> findMembersGroupByCustomer(Long idCustomer);

	/**
	 * Find members organisation by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<CustomerMemberDTO> findMembersOrganisationByCustomer(Long idCustomer);

	/**
	 * Find relationship by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<CustomerMemberDTO> findRelationshipByCustomer(Long idCustomer);

	/**
	 * Find customer active account.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @param idProduct the id product
	 * @return the long
	 */
	Long findCustomerActiveAccount(Long idCustomer, Long idProduct);

	/**
	 * Find arrears by customer.
	 *
	 * @author Salmen Fatnassi
	 * @param idCustomer the id customer
	 * @return the arrears DTO
	 */
	ArrearsDTO findArrearsByCustomer(Long idCustomer);

	/**
	 * Find all active accounts for customer.
	 *
	 * @author MoezMhiri
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(Long idCustomer);

	/**
	 * Find customer paid account.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @param idProduct the id product
	 * @return the double
	 */
	Double findCustomerPaidAccount(Long idCustomer, Long idProduct);

	/**
	 * Find customer paid accounts.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<Double> findCustomerPaidAccounts(Long idCustomer);

	/**
	 * Load check PAIMENT for given customer => return Map grouped by LoanID.
	 *
	 * @author HaythemBenizid
	 * @param customerNumber the customer number
	 * @return the map
	 */
	Map<Long, List<ScheduleDTO>> loadCheckPaiment(String customerNumber);

	
	/**
	 * Find loan details informations by loan.
	 *
	 * @param idLoan the id loan
	 * @return the list
	 */
	List<LoanDetailsInfoResponseDTO> findLoanDetailsInformationsByLoan(Long idLoan);

}
