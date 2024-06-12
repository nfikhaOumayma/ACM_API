/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import java.util.List;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.ChargeFeesDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerDetailsReportsDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingListDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.UsersNotificationsDTO;
import com.acm.utils.enums.ThirdPartyCategory;
import com.acm.utils.models.Loan;
import com.acm.utils.models.ThirdPartyMappingData;
import com.acm.utils.models.UserDefinedFieldsLinks;

// TODO: Auto-generated Javadoc
/**
 * The {@link CreditClient} Interface. to inject in order to consume services from credit-service to
 * be used in Batch service.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
@FeignClient(value = "credit-service", configuration = ClientConfiguration.class, decode404 = true)
@RibbonClient(name = "credit-service", configuration = LoadbalancerRuleFeignConfiguration.class)
public interface CreditClient {

	/**
	 * Find CustomerF by id => Full DATA.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the customer DTO
	 */
	@GetMapping("/customers/{id}")
	CustomerDTO findById(@PathVariable("id") Long id);

	/**
	 * Find CustomerDTO by given ID => only customer DATA .
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the customer DTO
	 */
	@GetMapping("/customers/find-ustomer/{id}")
	CustomerDTO findCustomerById(@PathVariable("id") Long id);

	/**
	 * Update the Customer by id =>Used in transvers-service).
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 */
	@PutMapping("/customers/update")
	CustomerDTO update(@RequestBody CustomerDTO customerDTO);

	/**
	 * Update meza card status.
	 * 
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 */
	@PostMapping("/customers/update-meza-card-status")
	CustomerDTO updateMezaCardStatus(@RequestBody CustomerDTO customerDTO);

	/**
	 * Find CustomerDTO by given mezaCardStatus .
	 *
	 * @author HaythemBenizid
	 * @param mezaCardStatus the meza card status
	 * @return the customer DTO
	 */
	@GetMapping("/customers/find-by-mezcard-status/{mezaCardStatus}")
	List<CustomerDTO> findByMezCardStatus(@PathVariable("mezaCardStatus") String mezaCardStatus);

	/**
	 * Find {@link ReportingListDTO} by Requested params {@link reportingDTO}.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the reporting list DTO
	 */
	@PostMapping("/loans-reporting/reporting-loan-application")
	ReportingListDTO find(@RequestBody ReportingDTO reportingDTO);

	/**
	 * Creates the {@link UserDefinedFieldsLinks} .
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the user defined fields links DTO
	 */
	@PostMapping("/udf-links/create-by-batch")
	UserDefinedFieldsLinksDTO createByBatch(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * Find UDF group by.
	 *
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the list
	 */
	@PostMapping("/udf-links/find-udf-groupby")
	List<UDFLinksGroupeFieldsDTO> findUDFGroupBy(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * Find ThirdPartyHistorique by given {@link ThirdPartyCategory} and ID Loan and ID customer for
	 * given category =(CUSTOMER / GUARANTOR).
	 *
	 * @author HaythemBenizid
	 * @param category the category
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @param categoryCustomer the category customer
	 * @return the list
	 */
	@GetMapping("/third-party-historiques/{category}/{idLoan}/{idCustomer}/{categoryCustomer}")
	List<ThirdPartyHistoriqueDTO> find(@PathVariable("category") String category,
			@PathVariable("idLoan") Long idLoan, @PathVariable("idCustomer") Long idCustomer,
			@PathVariable("categoryCustomer") String categoryCustomer);

	/**
	 * update config for all user => Enable/disable notification.
	 *
	 * @author ManelLamloum
	 * @param idSettingNotifications the id setting notifications
	 * @param statut the statut
	 * @return the list
	 */
	@PutMapping("/user-notifications/update-all/{idSettingNotifications}/{statut}")
	List<UsersNotificationsDTO> updateAll(
			@PathVariable("idSettingNotifications") Long idSettingNotifications,
			@PathVariable("statut") Boolean statut);

	/**
	 * Find loan by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the loan DTO
	 */
	@GetMapping("/loans/{id}")
	LoanDTO findLoanById(@PathVariable("id") Long id);

	/**
	 * Find document by id.
	 * 
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the documents loan DTO
	 */
	@GetMapping("/loans-documents/{id}")
	AcmDocumentsDTO findAcmDocumentsById(@PathVariable("id") Long id);

	/**
	 * Update the AcmDocuments by id.
	 *
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acmDocuments DTO
	 * @return the acmDocuments DTO
	 */
	@PutMapping("/loans-documents/update")
	AcmDocumentsDTO updateAcmDocuments(@RequestBody AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * Creates the notificationsDTO.
	 * 
	 * @author idridi
	 * @param notificationsDTO the notifications DTO
	 * @return the notifications DTO
	 */
	@PostMapping("/notifications/create")
	NotificationsDTO create(@RequestBody NotificationsDTO notificationsDTO);

	/**
	 * Find list document by given params.
	 * 
	 * @author ymezrani
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/loans-documents/create")
	AcmDocumentsDTO createAcmDocuments(@RequestBody AcmDocumentsDTO documentsLoanDTO);

	/**
	 * Find list document by given params.
	 * 
	 * @author HaythemBenizid
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/loans-documents/")
	List<AcmDocumentsDTO> find(@RequestBody AcmDocumentsDTO documentsLoanDTO);

	/**
	 * Save to ged.
	 *
	 * @param uploadedFiles the uploaded files
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the list
	 */
	@PostMapping("/loans-documents/saveToGed")
	List<AcmDocumentsDTO> saveToGed(@RequestParam("uploadedFiles") MultipartFile[] uploadedFiles,
			@RequestParam("documentsLoanDTO") String documentsLoanDTO);

	/**
	 * Create {@link Loan} method used by the BATCH to save LOANS in ACM DB.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("/loans/create")
	LoanDTO createByBatch(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Create the Address.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @param token the token
	 * @return the Address DTO
	 */
	@PostMapping("/address/create-by-batch")
	AddressDTO createByBatch(@RequestBody AddressDTO addressDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Creates the by batch.
	 * 
	 * @author idridi
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 */
	@PostMapping("/address/create-by-batch")
	AddressDTO createByBatch(@RequestBody AddressDTO addressDTO);

	/**
	 * Create transvershistorique =>Used in transvers-service).
	 *
	 * @author MoezMhiri
	 * @param transversHistoriqueDTO the transvers historique DTO
	 * @return the transversHistorique DTO
	 */
	@PostMapping("/transvers-historique/create")
	TransversHistoriqueDTO create(@RequestBody TransversHistoriqueDTO transversHistoriqueDTO);

	/**
	 * Find customer id extern.
	 * 
	 * @author idridi
	 * @param idExternCustomer the id extern customer
	 * @return the list
	 */
	@GetMapping("/customers/find-customer-by-id-extern/{idExternCustomer}")
	List<CustomerDTO> findCustomerIdExtern(@PathVariable("idExternCustomer") Long idExternCustomer);

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 */
	@PostMapping("/customers/create")
	CustomerDTO createCustomer(@RequestBody CustomerDTO customerDTO);

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	@PostMapping("/customer-link-relationship/")
	List<CustomerLinksRelationshipDTO> find(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the customer links relationship DTO
	 */
	@PostMapping("/customer-link-relationship/create")
	CustomerLinksRelationshipDTO create(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the acm documents DTO
	 */
	@PutMapping("/customer-link-relationship/update")
	CustomerLinksRelationshipDTO update(
			@RequestBody CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Update customers branches.
	 * 
	 * @author mlamloum
	 * @param customerDTOs the customerDTos
	 */
	@PutMapping("/customers/update-customers-branches")
	void updateCustomersBranches(@RequestBody List<CustomerDTO> customerDTOs);

	/**
	 * Update loan branches.
	 *
	 * @param customerDTOs the customer DT os
	 */
	@PutMapping("/loans/update-loan-branches")
	void updateLoanBranches(@RequestBody List<CustomerDTO> customerDTOs);

	/**
	 * Update all loan.
	 *
	 * @param loanDTOs the loan DT os
	 * @param action the action
	 */
	@PutMapping("/loans/update-all-loan/{action}")
	void updateAllLoan(@RequestBody List<LoanDTO> loanDTOs, @PathVariable("action") String action);

	/**
	 * Update all customer.
	 *
	 * @param customerDTOs the customer DT os
	 */
	@PutMapping("/customers/update-all-customer")
	void updateAllCustomer(@RequestBody List<CustomerDTO> customerDTOs);

	/**
	 * Find loan by id account extern.
	 *
	 * @param idAccountExtern the id account extern
	 * @param token the token
	 * @return the loan DTO
	 */

	@GetMapping("/loans/find-by-idAccountExtern/{idAccountExtern}")
	LoanDTO findLoanByIdAccountExtern(@PathVariable("idAccountExtern") Long idAccountExtern,
			@RequestHeader("Authorization") String token);

	/**
	 * Gets the guarantors details.
	 * 
	 * @author idridi
	 * @param idLoan the id loan
	 * @return the guarantors details
	 */
	@GetMapping("/customers/find-guarantors-details-by-idLoan/{idLoan}")
	CustomerDetailsReportsDTO getGuarantorsDetails(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find by id extern.
	 *
	 * @author idridi
	 * @param idExtern the id extern
	 * @param token the token
	 * @return the loan DTO
	 */
	@GetMapping("/loans/find-by-idExtern/{idExtern}")
	LoanDTO findByIdExtern(@PathVariable("idExtern") Long idExtern,
			@RequestHeader("Authorization") String token);

	/**
	 * Find by id extern.
	 *
	 * @param idExtern the id extern
	 * @return the loan DTO
	 */
	@GetMapping("/loans/find-by-idExtern/{idExtern}")
	LoanDTO findByIdExtern(@PathVariable("idExtern") Long idExtern);

	/**
	 * Count topups by account.
	 *
	 * @author mlamloum
	 * @param accountId the account id
	 * @return the integer
	 */
	@GetMapping("/loans/count-topup-by-acccount/{accountId}")
	Integer countTopupsByAccount(@PathVariable("accountId") Long accountId);

	/**
	 * Find loan.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the integer
	 */
	@PostMapping("/loans/")
	List<LoanDTO> findLoan(@RequestBody LoanDTO loanDTO);

	/**
	 * Save third party historique.
	 *
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the list
	 */
	@PostMapping("/third-party-historiques/save")
	ThirdPartyHistoriqueDTO saveThirdPartyHistorique(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO);

	/**
	 * Save third party historique with token.
	 *
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @param token the token
	 * @return the third party historique DTO
	 */
	@PostMapping("/third-party-historiques/saveWithToken")
	ThirdPartyHistoriqueDTO saveThirdPartyHistoriqueWithToken(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Gets the third party mapping.
	 *
	 * @author kouali
	 * @param nationality the nationality
	 * @param category the category
	 * @return the third party mapping
	 */
	@GetMapping("/third-party/mapping-third-party/{nationality}/{category}")
	ThirdPartyMappingData getThirdPartyMapping(@PathVariable("nationality") String nationality,
			@PathVariable("category") String category);

	/**
	 * Creates the.
	 *
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the third party historique DTO
	 */
	@PostMapping("/third-party-historiques/create")
	ThirdPartyHistoriqueDTO create(@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO);

	/**
	 * Find all customer information.
	 * 
	 * @author mlamloum
	 * @param customerDTO the customer DTO
	 * @param token the token
	 * @return the list
	 */
	@PostMapping("/load-data-ib/customers/get-all-customer-information")
	List<CustomerDTO> findAllCustomerInformationInIb(@RequestBody CustomerDTO customerDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Update loan.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 */
	@PostMapping("/load-data-ib/loans/update")
	void updateLoanInIb(@RequestBody LoanDTO loanDTO, @RequestHeader("Authorization") String token);

	/**
	 * Find acm loan.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the list
	 */
	@PostMapping("/load-data-ib/loans")
	List<LoanDTO> findLoanInIb(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Find customer by ib customer id.
	 * 
	 * @author mlamloum
	 * @param customerDTO the customer DTO
	 * @param token the token
	 * @return the list
	 */
	@GetMapping("customers/find-ustomer-by-ib-customer-id/{ibCustomerId}")
	List<CustomerDTO> findCustomerByIbCustomerId(@PathVariable("ibCustomerId") Long customerDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Find by id ib loan.
	 * 
	 * @author mlamloum
	 * @param idIbLoan the id ib loan
	 * @param token the token
	 * @return the list
	 */
	@GetMapping("loans/find-by-idIbLoan/{idIbLoan}")
	List<LoanDTO> findByIdIbLoan(@PathVariable("idIbLoan") Long idIbLoan,
			@RequestHeader("Authorization") String token);

	/**
	 * Find loan.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the list
	 */
	@PostMapping("loans/")
	List<LoanDTO> findLoan(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Find by search query id.
	 *
	 * @param token the token
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("loans/findBySearchQueryId")
	List<LoanDTO> findBySearchQueryId(@RequestHeader("Authorization") String token,
			@RequestBody LoanDTO loanDTO);

	/**
	 * Loan update for application.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */

	@PutMapping("loans/update-loan")
	LoanDTO updateLoan(@RequestBody LoanDTO loanDTO, @RequestHeader("Authorization") String token);

	/**
	 * Customer update for application.
	 * 
	 * @author mlamloum
	 * @param customerDTO the customer DTO
	 * @param token the token
	 * @return the customer DTO
	 */
	@PutMapping("/customers/update-for-application")
	CustomerDTO customerUpdateForApplication(@RequestBody CustomerDTO customerDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Loan save for application.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("/loans/create-to-abacus")
	LoanDTO loanSaveForApplication(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Customer save for application.
	 * 
	 * @author mlamloum
	 * @param customerDTO the customer DTO
	 * @param token the token
	 * @return the customer DTO
	 */
	@PostMapping("/customers/save-for-application")
	CustomerDTO customerSaveForApplication(@RequestBody CustomerDTO customerDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Validate.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("loans/validateWithToken")
	LoanDTO validate(@RequestBody LoanDTO loanDTO, @RequestHeader("Authorization") String token);

	/**
	 * Validate.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("loans/rejectedWithToken")
	LoanDTO rejectLoan(@RequestBody LoanDTO loanDTO, @RequestHeader("Authorization") String token);

	/**
	 * Job loans sanad.
	 *
	 * @param token the token
	 */
	@GetMapping("/loans/get-sanad-loan-automatic")
	void jobLoansSanad(@RequestHeader("Authorization") String token);

	/**
	 * Update acm loan and customer in IB.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("/load-data-ib/update-loan-and-customer")
	LoanDTO updateAcmLoanAndCustomerInIB(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Validate loan.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("/loans/validate")
	LoanDTO validateLoan(@RequestBody LoanDTO loanDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Save loans schedules.
	 *
	 * @param loanScheduleDTO the loan schedule DTO
	 * @param token the token
	 * @return the loan schedule DTO
	 */
	@PostMapping("/load-data-ib/loan/schedules")
	LoanScheduleDTO saveLoansSchedules(@RequestBody LoanScheduleDTO loanScheduleDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Find customer by ib id.
	 *
	 * @param ibCustomerId the ib customer id
	 * @param token the token
	 * @return the list
	 */
	@GetMapping("/customers/find-ustomer-by-ib-customer-id/{ibCustomerId}")
	List<CustomerDTO> findCustomerByIbId(@PathVariable("ibCustomerId") Long ibCustomerId,
			@RequestHeader("Authorization") String token);

	/**
	 * Cancel loan.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return the loan DTO
	 */
	@PostMapping("/loans/cancelled")
	LoanDTO cancelLoan(@RequestBody LoanDTO loanDTO, @RequestHeader("Authorization") String token);

	/**
	 * Creates the.
	 *
	 * @param transversHistoriqueDTO the transvers historique DTO
	 * @param token the token
	 * @return the transvers historique DTO
	 */
	@PostMapping("/transvers-historique/create")
	TransversHistoriqueDTO create(@RequestBody TransversHistoriqueDTO transversHistoriqueDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Find loan third party historique with token.
	 *
	 * @param searchQueryId the search query id
	 * @param category the category
	 * @param token the token
	 * @return the loan DTO
	 */
	@GetMapping("/third-party-historiques/find-with-token/by-search-query-id/{searchQueryId}/{category}")
	LoanDTO findLoanThirdPartyHistoriqueWithToken(@PathVariable("searchQueryId") Long searchQueryId,
			@PathVariable("category") String category,
			@RequestHeader("Authorization") String token);

	/**
	 * Find loan third party historique by customer reis with token.
	 *
	 * @param customerIdReis the customer id reis
	 * @param category the category
	 * @param token the token
	 * @return the loan DTO
	 */
	@GetMapping("/third-party-historiques/find-with-token/by-customer-reis-id/{customerIdReis}/{category}")
	LoanDTO findLoanThirdPartyHistoriqueByCustomerReisWithToken(
			@PathVariable("customerIdReis") Long customerIdReis,
			@PathVariable("category") String category,
			@RequestHeader("Authorization") String token);

	/**
	 * Disable document.
	 *
	 * @param documentsLoanDTO the documents loan DTO
	 */
	@PutMapping("/loans-documents/disable-document")
	void disableDocument(@RequestBody AcmDocumentsDTO documentsLoanDTO);

	/**
	 * Delete document.
	 *
	 * @param id the id
	 */
	@DeleteMapping("/loans-documents/delete/{id}")
	void deleteDocument(@PathVariable("id") Long id);

	/**
	 * Update udf links by element id.
	 * 
	 * @author mlamloum
	 * @param userDefinedFieldsLinksDTOs the user defined fields links DT os
	 * @param elementId the element id
	 */
	@PostMapping("/udf-links/update-udf-links-by-elementId/{elementId}")
	void updateUdfLinksByElementId(
			@RequestBody List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs,
			@PathVariable("elementId") Long elementId);

	/**
	 * Sync IB setting from ACM.
	 *
	 * @param productsACM the products ACM
	 * @return the string
	 */
	@PostMapping("/load-data-ib/reload-setting")
	String syncIBSettingFromACM(@RequestBody List<ProductDTO> productsACM);

	/**
	 * Login api ib.
	 *
	 * @return the string
	 */
	@GetMapping("/load-data-ib/ib-token")
	String loginApiIb();

	/**
	 * Count conditionnal approve by item.
	 *
	 * @param idItem the id item
	 * @return the long
	 */
	@GetMapping("/conditionalApprove/count/item/{idItem}")
	Long countConditionnalApproveByItem(@PathVariable("idItem") Long idItem);

	/**
	 * Find.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the list
	 */
	@PostMapping("/charge-fees/")
	List<ChargeFeesDTO> findChargeFees(@RequestBody ChargeFeesDTO chargeFeesDTO);

	/**
	 * Creates the all.
	 *
	 * @param ChargeFeesDTOs the charge fees DT os
	 * @return the list
	 */
	@PostMapping("/charge-fees/create-all")
	List<ChargeFeesDTO> createAllChargeFees(@RequestBody List<ChargeFeesDTO> ChargeFeesDTOs);

}
