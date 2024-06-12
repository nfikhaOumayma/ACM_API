/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CheckFieldsConfigurationException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.MezaCardExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.CustomerService;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerDetailsReportsDTO;
import com.acm.utils.dtos.CustomerMezaCardStatutDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.CustomerPaginationDTO;
import com.acm.utils.enums.CustomerMezaCardStatus;
import com.acm.utils.validation.ACMValidationUtils;

import io.opentracing.Span;
import io.opentracing.Tracer;

/**
 * This class @{link CustomerController} used to control all the Customer requests.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@RestController
@RequestMapping("/customers")
public class CustomerController {

	/** The Customer service. */
	@Autowired
	private CustomerService customerService;

	/** The tracer. */
	@Autowired
	private Tracer tracer;

	/**
	 * Find Custome by id=> FULL data with list Address && UDFs.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public CustomerDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		if (!ACMValidationUtils.isNullOrEmpty(id)) {
			return customerService.find(id);
		}
		else {
			return new CustomerDTO();
		}
	}

	/**
	 * Find by id for claims.
	 *
	 * @param id the id
	 * @return the customer DTO
	 */
	@GetMapping("forClaims/{id}")
	public CustomerDTO findByIdForClaims(@PathVariable("id") Long id) {

		if (!ACMValidationUtils.isNullOrEmpty(id)) {
			return customerService.findForClaims(id);
		}
		else {
			return new CustomerDTO();
		}
	}

	/**
	 * Find CustomerDTO by given ID => only customer DATA .
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-ustomer/{id}")
	public CustomerDTO findCustomer(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return customerService.findCustomer(id);
	}

	/**
	 * Find customer by ib customer id.
	 *
	 * @param ibCustomerId the ib customer id
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-ustomer-by-ib-customer-id/{ibCustomerId}")
	public List<CustomerDTO> findCustomerByIbCustomerId(
			@PathVariable("ibCustomerId") Long ibCustomerId) throws ResourcesNotFoundException {

		return customerService.findCustomerByIbCustomerId(ibCustomerId);
	}

	/**
	 * Find {@link List} of {@link CustomerDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<CustomerDTO> find(@RequestBody CustomerDTO customerDTO) {

		return customerService.find(customerDTO);
	}

	/**
	 * Find {@link List} of {@link CustomerDTO} list of customerName by given loan owner which is
	 * the user connected and which given user is responsable.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-customers")
	public List<CustomerDTO> findCustomers() {

		return customerService.findCustomers();
	}

	/**
	 * Create the Customer.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the Customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	@PostMapping("/create")
	public CustomerDTO create(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CalculateAgeException {

		return customerService.save(customerDTO);
	}

	/**
	 * Update the Customer by id.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	@PutMapping("/update")
	public CustomerDTO update(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		return customerService.save(customerDTO.getId(), customerDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author YesserSomai
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	@PostMapping("/find-pagination")
	public CustomerPaginationDTO findPagination(
			@RequestBody CustomerPaginationDTO customerPaginationDTO) {

		return customerService.find(customerPaginationDTO);
	}

	/**
	 * Find for link by given params && link category : (GUARANTOR / GRP / ORG / RELATIONSHIP).
	 *
	 * @author HaythemBenizid
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	@PostMapping("/find-pagination-for-link")
	public CustomerPaginationDTO findForLink(
			@RequestBody CustomerPaginationDTO customerPaginationDTO) {

		return customerService.findForLink(customerPaginationDTO);
	}

	/**
	 * Find list of findCustomerAccount by given loan ID.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the Customer Id
	 * @return the list of Customer Account
	 */
	@GetMapping("/customer-account/{idcustomer}")
	public List<CustomerAccountDTO> findCustomerAccount(
			@PathVariable("idcustomer") Long idCustomer) {

		return customerService.findCustomerAccount(idCustomer);
	}

	/**
	 * The method used to check duplication customers and save the given {@link CustomerDTO} in ACM
	 * DB and save in ABACUS DB with API.
	 *
	 * @author YesserSomai
	 * @param customerDTO the customer DTO
	 * @return customerDTO the customer DTO for save
	 * @throws CreditException the credit exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws CheckFieldsConfigurationException the check fields configuration exception
	 * @throws MezaCardExistException the meza card exist exception
	 */
	@PostMapping("/save-for-application")
	public CustomerDTO saveForApplication(@RequestBody CustomerDTO customerDTO)
			throws CreditException, ResourcesNotFoundException, ApiAbacusException, IOException,
			CalculateAgeException, CustomerMaxActiveAccountException,
			CheckFieldsConfigurationException, MezaCardExistException {

		Span controllerSpan = tracer.buildSpan("Credit-Service/save-for-application: Controller")
				.withTag("Customer", "Customer Save for Application Tracking").start();

		CustomerDTO saveCustomerDTO = customerService.saveForApplication(customerDTO);
		controllerSpan.finish();

		return saveCustomerDTO;
	}

	/**
	 * Update for application.
	 *
	 * @author MoezMhiri
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws MezaCardExistException the meza card exist exception
	 */
	@PutMapping("/update-for-application")
	public CustomerDTO updateForApplication(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CreditException, ApiAbacusException, IOException,
			CalculateAgeException, MezaCardExistException {

		Span controllerSpan = tracer.buildSpan("Credit-Service/save-for-application: Controller")
				.withTag("Customer", "Customer Save for Application Tracking").start();
		CustomerDTO updateCustomerDTO = customerService.updateForApplication(customerDTO);
		controllerSpan.finish();
		return updateCustomerDTO;
	}

	/**
	 * Find customer members details.
	 * 
	 * @author MoezMhiri
	 * @param customerDTO the customerDTO
	 * @return the list CustomerDTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-customer-by-members")
	public List<CustomerDTO> findCustomerMember(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		return customerService.findCustomersRelationShip(customerDTO);
	}

	/**
	 * Load all.
	 *
	 * @author MOEZ
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	@GetMapping("/load-all")
	public void loadAll() throws CalculateAgeException, CreditException {

		customerService.loadAll();
	}

	/**
	 * Find customer active account.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @param idproduct the idproduct
	 * @return the long
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/customer-active-account/{idcustomer}/{idproduct}")
	public Long findCustomerActiveAccount(@PathVariable("idcustomer") Long idCustomer,
			@PathVariable("idproduct") Long idproduct) throws ResourcesNotFoundException {

		return customerService.findCustomerActiveAccount(idCustomer, idproduct);
	}

	/**
	 * Find customer active account.
	 *
	 * @param idCustomer the id customer
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-all-active-accounts-for-customer/{idcustomer}")
	public List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(
			@PathVariable("idcustomer") Long idCustomer) throws ResourcesNotFoundException {

		return customerService.findAllActiveAccountsForCustomer(idCustomer);
	}

	/**
	 * Find details arrears by ID customer extern from ABACUS DB.
	 *
	 * @author Salmen Fatnassi
	 * @param idCustomerExtern the id customer extern
	 * @return the arrears DTO
	 */
	@GetMapping("/data-abacus/customer-arrears/{idCustomerExtern}")
	public ArrearsDTO findArrearsCustomer(@PathVariable("idCustomerExtern") Long idCustomerExtern) {

		return customerService.findArrearsCustomer(idCustomerExtern);
	}

	/**
	 * Resend login.
	 *
	 * @author MoezMhiri
	 * @param customerDTO the customer DTO
	 * @return the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/resend-login")
	public UserDTO resendLogin(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		return customerService.resendLogin(customerDTO);
	}

	/**
	 * Upload customer photo.
	 *
	 * @author YessesSomai
	 * @param photo the photo
	 * @param idCustomer the id customer
	 * @return the byte[]
	 * @throws GEDException the GED exception
	 * @throws FileNotFoundException the file not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping(path = "/upload-photo/")
	public byte[] uploadCustomerPhoto(@RequestBody MultipartFile photo,
			@RequestParam String idCustomer)
			throws GEDException, FileNotFoundException, ResourcesNotFoundException {

		return customerService.uploadCustomerPhoto(photo, idCustomer);
	}

	/**
	 * Find photo customer.
	 *
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/photo-customer/{idCustomer}")
	public byte[] findPhotoCustomer(@PathVariable("idCustomer") String idCustomer)
			throws ResourcesNotFoundException {

		return customerService.findPhotoCustomer(idCustomer);
	}

	/**
	 * Check customer loan issued.
	 *
	 * @author moezMhiri
	 * @param customerDTO the customer DTO
	 * @return the boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/check-loan-issued")
	public Boolean checkCustomerLoanIssued(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		return customerService.checkCustomerLoanStatuts(customerDTO);
	}

	/**
	 * Find customer paid account.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @param idproduct the idproduct
	 * @return the double
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/customer-paid-account/{idcustomer}/{idproduct}")
	public Double findCustomerPaidAccount(@PathVariable("idcustomer") Long idCustomer,
			@PathVariable("idproduct") Long idproduct) throws ResourcesNotFoundException {

		return customerService.findCustomerPaidAccount(idCustomer, idproduct);
	}

	/**
	 * Update meza card status : {@link CustomerMezaCardStatus}.
	 * 
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-meza-card-status")
	public CustomerDTO updateMezaCardStatus(@RequestBody CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		return customerService.updateMezaCardStatus(customerDTO);
	}

	/**
	 * Find CustomerDTO by given mezaCardStatus .
	 *
	 * @author HaythemBenizid
	 * @param mezaCardStatus the meza card status
	 * @return the customer DTO
	 */
	@GetMapping("/find-by-mezcard-status/{mezaCardStatus}")
	public List<CustomerDTO> findByMezCardStatus(
			@PathVariable("mezaCardStatus") String mezaCardStatus) {

		return customerService.findByMezCardStatus(mezaCardStatus);
	}

	/**
	 * Find for meza card.
	 * 
	 * @author MoezMhiri
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	@PostMapping("/find-pagination-for-meza-card")
	public CustomerPaginationDTO findForMezaCard(
			@RequestBody CustomerPaginationDTO customerPaginationDTO) {

		return customerService.findForMezaCard(customerPaginationDTO);
	}

	/**
	 * Count.
	 *
	 * @author MoezMhiri
	 * @return the customer meza card statut DTO
	 */
	@GetMapping("/count")
	public CustomerMezaCardStatutDTO count() {

		return customerService.count();
	}

	/**
	 * update All.
	 *
	 * @author MoezMhiri
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-all-customer-meza-card")
	public void updateAll(@RequestBody List<CustomerDTO> customerDTOs)
			throws ResourcesNotFoundException {

		customerService.updateAll(customerDTOs);
	}

	/**
	 * Find customer id extern.
	 * 
	 * @author idridi
	 * @param idExternCustomer the id extern customer
	 * @return the list
	 */
	@GetMapping("/find-customer-by-id-extern/{idExternCustomer}")
	public List<CustomerDTO> findCustomerIdExtern(
			@PathVariable("idExternCustomer") Long idExternCustomer) {

		return customerService.findCustomerIdExtern(idExternCustomer, null);

	}

	/**
	 * Update customers branches.
	 * 
	 * @author mlamloum
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-customers-branches")
	public void updateCustomersBranches(@RequestBody List<CustomerDTO> customerDTOs)
			throws ResourcesNotFoundException {

		customerService.updateCustomersBranches(customerDTOs);
	}

	/**
	 * Update allcustomer.
	 *
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-all-customer")
	public void updateAllcustomer(@RequestBody List<CustomerDTO> customerDTOs)
			throws ResourcesNotFoundException {

		customerService.updateAllCustomer(customerDTOs);
	}

	/**
	 * Gets the guarantors details.
	 * 
	 * @author idridi
	 * @param idLoan the id loan
	 * @return the guarantors details
	 */
	@GetMapping("/find-guarantors-details-by-idLoan/{idLoan}")
	public CustomerDetailsReportsDTO getGuarantorsDetails(@PathVariable("idLoan") Long idLoan) {

		return customerService.getGuarantorsDetails(idLoan);
	}

	/**
	 * Find customer id extern.
	 *
	 * @author idridi
	 * @param idExternCustomer the id extern customer
	 * @param accountNumberExtern the account number extern
	 * @return the list
	 */
	@GetMapping("/find-schedule-by-customer/{idExternCustomer}/{accountNumberExtern}")
	public List<ScheduleDTO> findAccountScheduleByCustomerId(
			@PathVariable("idExternCustomer") Long idExternCustomer,
			@PathVariable("accountNumberExtern") String accountNumberExtern) {

		return customerService.findAccountScheduleByCustomerId(idExternCustomer,
				accountNumberExtern);

	}
}
