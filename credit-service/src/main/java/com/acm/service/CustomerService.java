/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CheckFieldsConfigurationException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.MezaCardExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerDetailsReportsDTO;
import com.acm.utils.dtos.CustomerMezaCardStatutDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.CustomerPaginationDTO;
import com.acm.utils.enums.CustomerMezaCardStatus;

/**
 * {@link CustomerService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public interface CustomerService {

	/**
	 * Find {@link CustomerDTO} by given ID. Find {@link CustomerDTO} by given ID => FULL data with
	 * list Address && UDFs.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find for claims.
	 *
	 * @param id the id
	 * @return the customer DTO
	 */
	CustomerDTO findForClaims(Long id);

	/**
	 * Find {@link CustomerDTO} by given ID => only customer DATA .
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDTO findCustomer(Long id) throws ResourcesNotFoundException;

	/**
	 * Find customer by ib customer id.
	 *
	 * @author mlamloum
	 * @param ibCustomerId the ib customer id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<CustomerDTO> findCustomerByIbCustomerId(Long ibCustomerId)
			throws ResourcesNotFoundException;

	/**
	 * Find {@link CustomerDTO} by given customerIdExtern (USED by Batch LOAN).
	 *
	 * @author HaythemBenizid
	 * @param customerIdExtern the customer id extern
	 * @param getAllData the get all data
	 * @return the list
	 */
	List<CustomerDTO> findCustomerIdExtern(Long customerIdExtern, Optional<Boolean> getAllData);

	/**
	 * Find list {@link CustomerDTO} by given status {@link CustomerMezaCardStatus}.
	 *
	 * @author HaythemBenizid
	 * @param customerMezaCardStatus the customer meza card status
	 * @return the list
	 */
	List<CustomerDTO> findByMezCardStatus(String customerMezaCardStatus);

	/**
	 * Find {@link List} of {@link CustomerDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	List<CustomerDTO> find(CustomerDTO customerDTO);

	/**
	 * The method used for saving the given {@link CustomerDTO}.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDTO save(CustomerDTO customerDTO)
			throws CalculateAgeException, ResourcesNotFoundException;

	/**
	 * The method used for updating the given {@link CustomerDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	CustomerDTO save(Long id, CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CalculateAgeException, CreditException;

	/**
	 * Find {@link List} of {@link CustomerDTO} list of customerName by given customer owner which
	 * is the user connected and which given user is responsable.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<CustomerDTO> findCustomers();

	/**
	 * Find {@link CustomerPaginationDTO} by page size & page number & given params
	 * ({@link CustomerPaginationDTO}).
	 * 
	 * @author YesserSomai
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the list
	 */
	CustomerPaginationDTO find(CustomerPaginationDTO customerPaginationDTO);

	/**
	 * Find {@link findCustomerAccount}.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the customer id
	 * @return the list
	 */
	List<CustomerAccountDTO> findCustomerAccount(Long idCustomer);

	/**
	 * The method used to check duplication customers and save the given {@link CustomerDTO} in ACM
	 * DB and save in ABACUS DB with API.
	 *
	 * @author YesserSomai
	 * @param customerDTO the customer desicion DTO
	 * @return the CustomerService DTO
	 * @throws CreditException the credit exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws CheckFieldsConfigurationException the check fields configuration exception
	 * @throws MezaCardExistException the meza card exist exception
	 */
	CustomerDTO saveForApplication(CustomerDTO customerDTO)
			throws CreditException, ResourcesNotFoundException, ApiAbacusException, IOException,
			CalculateAgeException, CustomerMaxActiveAccountException,
			CheckFieldsConfigurationException, MezaCardExistException;

	/**
	 * The method used for updating the given {@link CustomerDTO} by ID.
	 *
	 * @author MoezMhiri
	 * @param customerDTO the customer
	 * @return the customerDTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws MezaCardExistException the meza card exist exception
	 */
	CustomerDTO updateForApplication(CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CreditException, ApiAbacusException, IOException,
			CalculateAgeException, MezaCardExistException;

	/**
	 * Find {@link findCustomerByRelationShip}.
	 * 
	 * @author MoezMhiri
	 * @param customerDTO the customer DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<CustomerDTO> findCustomersRelationShip(CustomerDTO customerDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find Guarantor {@link CustomerPaginationDTO} by Page size & Page number & given params
	 * {@link CustomerDTO} && given customer link category :( GUARANTOR / GRP / ORG / RELATIONSHIP)
	 * and return ({@link CustomerPaginationDTO}.
	 * 
	 * @author InesDridi
	 * @author HaythemBenizid
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the list
	 */
	CustomerPaginationDTO findForLink(CustomerPaginationDTO customerPaginationDTO);

	/**
	 * Adds the guarantors.
	 *
	 * @author Salmen Fatnassi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO addGuarantors(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Load all.
	 *
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	void loadAll() throws CalculateAgeException, CreditException;

	/**
	 * Find customer active account.
	 *
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @param idproduct the idproduct
	 * @return the long
	 */
	Long findCustomerActiveAccount(Long idCustomer, Long idproduct);

	/**
	 * Find arrears customer.
	 *
	 * @author Salmen Fatnassi
	 * @param idCustomer the id customer
	 * @return the arrears DTO
	 */
	ArrearsDTO findArrearsCustomer(Long idCustomer);

	/**
	 * Find all active accounts for customer.
	 *
	 * @author MoezMhiri
	 * @param idCustomer the id customer
	 * @return the list
	 */
	List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(Long idCustomer);

	/**
	 * Resend login.
	 *
	 * @author MoezMhiri
	 * @param customerDTO the customer DTO
	 * @return the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDTO resendLogin(CustomerDTO customerDTO) throws ResourcesNotFoundException;

	/**
	 * Upload customer photo.
	 *
	 * @author YesserSomai
	 * @param photo the photo
	 * @param idCustomer the id customer
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	byte[] uploadCustomerPhoto(MultipartFile photo, String idCustomer)
			throws ResourcesNotFoundException;

	/**
	 * Find photo customer.
	 *
	 * @param idCustomer the id customer
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	byte[] findPhotoCustomer(String idCustomer) throws ResourcesNotFoundException;

	/**
	 * Check customer loan statuts.
	 *
	 * @author moezMhiri
	 * @param customerDTO the customer DTO
	 * @return the boolean
	 */
	Boolean checkCustomerLoanStatuts(CustomerDTO customerDTO);

	/**
	 * Find customer paid account.
	 *
	 * @author idridi
	 * @param idCustomer the id customer
	 * @param idproduct the idproduct
	 * @return the double
	 */
	Double findCustomerPaidAccount(Long idCustomer, Long idproduct);

	/**
	 * Update meza card status : {@link CustomerMezaCardStatus}.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDTO updateMezaCardStatus(CustomerDTO customerDTO) throws ResourcesNotFoundException;

	/**
	 * Find for meza card.
	 *
	 * @author MoezMhiri
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	CustomerPaginationDTO findForMezaCard(CustomerPaginationDTO customerPaginationDTO);

	/**
	 * Count.
	 *
	 * @author MoezMhiri
	 * @return the customer meza card statut DTO
	 */
	CustomerMezaCardStatutDTO count();

	/**
	 * Update all.
	 *
	 * @author MoezMhiri
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateAll(List<CustomerDTO> customerDTOs) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link CustomerDTO} Analytics for connected user by given customer owner
	 * which is the user connected and which given user is RESPONSABLE BY MONTH.
	 *
	 * @author HaythemBenizid
	 * @param hasActiveLoan the has active loan
	 * @param firstDayMonth the first day month
	 * @param lastDayMonth the last day month
	 * @return the list
	 */
	Map<Integer, Integer> findForAnalytics(Boolean hasActiveLoan, LocalDate firstDayMonth,
			LocalDate lastDayMonth);

	/**
	 * Update customers branches.
	 * 
	 * @author mlamloum
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateCustomersBranches(List<CustomerDTO> customerDTOs) throws ResourcesNotFoundException;

	/**
	 * Update all customer.
	 *
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateAllCustomer(List<CustomerDTO> customerDTOs) throws ResourcesNotFoundException;

	/**
	 * Gets the guarantors details.
	 * 
	 * @author idridi
	 * @param idLoan the id loan
	 * @return the guarantors details
	 */
	CustomerDetailsReportsDTO getGuarantorsDetails(Long idLoan);

	/**
	 * Find account schedule by customer id.
	 *
	 * @param idExternCustomer the id extern customer
	 * @param accountNumberExtern the account number extern
	 * @return the list
	 */
	List<ScheduleDTO> findAccountScheduleByCustomerId(Long idExternCustomer,
			String accountNumberExtern);

}
