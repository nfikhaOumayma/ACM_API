/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.soap.kyc.model.KycDTO;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressTypeDTO;
import com.acm.utils.dtos.ApplicationFeeDTO;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.BranchChangeDTO;
import com.acm.utils.dtos.BrancheDTO;
import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;
import com.acm.utils.dtos.CollateralTypeDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.ConfirmPurchaseOrSaleRequestApiDTO;
import com.acm.utils.dtos.ConfirmPurchaseResponseApiDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.DeferredPeriodTypeDTO;
import com.acm.utils.dtos.DisburseDTO;
import com.acm.utils.dtos.DisburseResponse;
import com.acm.utils.dtos.EmploymentStatusInfoMasdrAPIDTO;
import com.acm.utils.dtos.ExpensesJournalPageDTO;
import com.acm.utils.dtos.FinancialAnalysisDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.IScoreDTO;
import com.acm.utils.dtos.IndustryDTO;
import com.acm.utils.dtos.JournalEnteriesToAbacusDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanGuarantorSourceDTO;
import com.acm.utils.dtos.LoanProcessSettingDTO;
import com.acm.utils.dtos.LoanRefinanceReasonDTO;
import com.acm.utils.dtos.LoanSourceOfFundsDTO;
import com.acm.utils.dtos.PaymentApiSanadDTO;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ProductLoanReasonsDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiRequestDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiResponseDTO;
import com.acm.utils.dtos.RelationshipDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingSchedulesStatusDTO;
import com.acm.utils.dtos.RequestEnquiryNewCustomerSimahApiDTO;
import com.acm.utils.dtos.RequestGetScoreDTO;
import com.acm.utils.dtos.ResponseChargeFeeDTO;
import com.acm.utils.dtos.ResponseIncomeDakhliApiDTO;
import com.acm.utils.dtos.RoleAbacusDTO;
import com.acm.utils.dtos.SaleMurabhaApiRequestDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.models.transvers.LoanScheduleAPI;
import com.vneuron.utils.dtos.CustomerVneuron;
import com.vneuron.utils.dtos.RiskResponse;
import com.vneuron.utils.dtos.SearchPersonCustomerResponse;

/**
 * The {@link TransversClient} Interface. to inject in order to consume services from
 * transvers-service
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@FeignClient(value = "transvers-service", configuration = ClientConfiguration.class,
		decode404 = true)
@RibbonClient(name = "transvers-service", configuration = LoadbalancerRuleFeignConfiguration.class)
public interface TransversClient {

	/**
	 * Find loan details by ID loan extern from ABACUS DB.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the loan DTO
	 */
	@GetMapping("/load-data-abacus/loan/{idLoan}")
	LoanDTO findLoan(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find all schedules by ID loan extern from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/loans/schedule/{idLoan}")
	List<ScheduleDTO> findSchedules(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find customer details by ID loan extern from ABACUS DB.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id Loan
	 * @return the customer DTO
	 */
	@GetMapping("/load-data-abacus/customer-loan/{idLoan}")
	CustomerDTO findCustomerByLoan(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find customer by given ID.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the customer DTO
	 */
	@GetMapping("/load-data-abacus/customer/{idCustomer}")
	CustomerDTO findCustomerById(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find customers for batch.
	 * 
	 * @author idridi
	 * @param idCustomerExtern the id customer extern
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/get-customers-for-batch/{idCustomerExtern}")
	List<CustomerDTO> findCustomersForBatch(
			@PathVariable("idCustomerExtern") Long idCustomerExtern);

	/**
	 * load all customer.
	 * 
	 * @author MOEZ
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/customer/all")
	List<CustomerDTO> load();

	/**
	 * Find customer previous account by ID loan extern from ABACUS DB.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list {@link CustomerAccountDTO}
	 */
	@GetMapping("/load-data-abacus/customer-account/{idLoan}")
	List<CustomerAccountDTO> findCustomerAccountByLoan(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find customer previous account by ID customer extern from ABACUS DB.
	 * 
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @return the list {@link CustomerAccountDTO}
	 */
	@GetMapping("/load-data-abacus/account-by-customer/{idCustomer}")
	List<CustomerAccountDTO> findCustomerAccountByCustomer(
			@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find customer account schedules by loan.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list {@link ScheduleDTO}
	 */
	@GetMapping("/load-data-abacus/customer-account-schedule/{idLoan}")
	List<ScheduleDTO> findCustomerAccountScheduleByLoan(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find members group by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-members-group/{idCustomer}")
	List<CustomerMemberDTO> findMembersGroupByCustomer(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find members organisation by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-members-organisation/{idCustomer}")
	List<CustomerMemberDTO> findMembersOrganisationByCustomer(
			@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find relationship by customer.
	 * 
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-relationship/{idCustomer}")
	List<CustomerMemberDTO> findRelationshipByCustomer(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find list of guarantors by given loan ID.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/loans/guarantors/{idLoan}")
	List<GuarantorDTO> findGuarantors(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find list of collaterol by given loan ID.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list of Collaterols
	 */
	@GetMapping("/load-data-abacus/loans/collaterols/{idLoan}")
	List<CollaterolDTO> findCollaterols(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find active and inactive collaterals.
	 *
	 * @author mlamloum
	 * @param idLoans the id loans
	 * @return the list
	 */
	@PostMapping("/load-data-abacus/loans/all-collaterols")
	List<AcmCollateralDTO> findActiveAndInactiveCollaterols(@RequestBody List<Long> idLoans);

	/**
	 * Find financialAnalysis by idLoan.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id Loan
	 * @return the financialAnalysis DTO
	 */
	@GetMapping("/load-data-abacus/financial-analysis/{idLoan}")
	List<FinancialAnalysisDTO> findFinancialAnalysis(@PathVariable("idLoan") Long idLoan);

	/**
	 * Find loan process setting from ABACUS DB by id product.
	 * 
	 * @author HaythemBenizid
	 * @param idProduct the id product
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/loans/process/{idProduct}")
	List<LoanProcessSettingDTO> findLoanProcessSetting(@PathVariable("idProduct") Long idProduct);

	/**
	 * Calculate loan schedules.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws Exception the exception
	 */
	@PostMapping("/load-data-api-abacus/calculate-loan-schedules")
	LoanScheduleAPI calculateLoanSchedules(@RequestBody LoanDTO loanDTO) throws Exception;

	/**
	 * Cancel loan.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 */
	@PostMapping("/load-data-api-abacus/cancel-loan")
	void cancelLoan(@RequestBody LoanDTO loanDTO);

	/**
	 * create INDIV loan using ABACUS API.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws Exception the exception
	 */
	@PostMapping("/load-data-api-abacus/create-loan-indiv")
	LoanDTO createLoanINDIV(@RequestBody LoanDTO loanDTO) throws Exception;

	/**
	 * Creates the loan GRP.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @return the list
	 */
	@PostMapping("/load-data-api-abacus/create-loan-grp")
	List<LoanDTO> createLoanGRP(@RequestBody List<LoanDTO> loanDTOs);

	/**
	 * Creates the loan ORG.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	@PostMapping("/load-data-api-abacus/create-loan-org")
	LoanDTO createLoanORG(@RequestBody LoanDTO loanDTO);

	/**
	 * Update loan INDIV - ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 */
	@PostMapping("/load-data-api-abacus/update-loan-indiv-org")
	void updateLoanINDIVORG(@RequestBody LoanDTO loanDTO);

	/**
	 * update the loan GRP.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 */
	@PostMapping("/load-data-api-abacus/update-loan-grp")
	void updateLoanGRP(@RequestBody List<LoanDTO> loanDTOs);

	/**
	 * Find product.
	 *
	 * @author YesserSomai
	 * @param productId the product id
	 * @return the product DTO
	 */
	@GetMapping("/load-data-abacus/product/{productId}")
	ProductDTO findProduct(@PathVariable("productId") Long productId);

	/**
	 * Find all Products from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/products/")
	List<ProductDTO> findProducts();

	/**
	 * Add Customer by given params in ABACUS-DB using API.
	 *
	 * @author YesserSomai
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws Exception the exception
	 */
	@PostMapping("/load-data-api-abacus/add-customer")
	CustomerDTO addCustomer(@RequestBody CustomerDTO customerDTO) throws Exception;

	/**
	 * Update customer.
	 *
	 * @author MoezMhiri
	 * @param customerDTO the customer DTO
	 * @throws Exception the exception
	 */
	@PostMapping("/load-data-api-abacus/update-customer")
	void updateCustomer(@RequestBody CustomerDTO customerDTO) throws Exception;

	/**
	 * Find fee repayment.
	 *
	 * @author Salmen Fatnassi
	 * @param idAccount the id account
	 * @return the long
	 */
	@GetMapping("/load-data-abacus/fee-repayment/{idAccount}")
	Long findFeeRepayment(@PathVariable("idAccount") Long idAccount);

	/**
	 * Check fee.
	 *
	 * @param idAccount the id account
	 * @param listIds the list ids
	 * @return the long
	 */
	@PostMapping("/load-data-abacus/check-fee/{idAccount}")
	Long checkFee(@PathVariable("idAccount") Long idAccount, @RequestBody List<Long> listIds);

	/**
	 * Find application fee.
	 *
	 * @author MoezMhiri
	 * @param idAccount the id account
	 * @return the long
	 */
	@GetMapping("/load-data-abacus/application-fee/{idAccount}")
	Long findApplicationFee(@PathVariable("idAccount") Long idAccount);

	/**
	 * Approve loan INDIV / ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 */
	@PostMapping("/load-data-api-abacus/approve-loan")
	void approveLoan(@RequestBody LoanDTO loanDTO);

	/**
	 * Approve loan GRP.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 */
	@PostMapping("/load-data-api-abacus/approve-loan-group")
	void approvelGroup(@RequestBody List<LoanDTO> loanDTOs);

	/**
	 * create the guarantor.
	 * 
	 * @author HaythemBenizid
	 * @param guarantorDTO the guarantor DTO
	 * @return the guarantor DTO
	 */
	@PostMapping("/load-data-api-abacus/add-guarantor")
	GuarantorDTO createGuarantor(@RequestBody GuarantorDTO guarantorDTO);

	/**
	 * Creates the guarantors.
	 *
	 * @author mlamloum
	 * @param guarantorDTO the guarantor DTO
	 */
	@PostMapping("/load-data-api-abacus/add-guarantors")
	void createGuarantors(@RequestBody List<GuarantorDTO> guarantorDTO);

	/**
	 * Find Customer Active Account.
	 *
	 * @author YesserSomai
	 * @param idCustomer the id customer
	 * @param idProduct the id product
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-customer-active-account/{idCustomer}/{idProduct}")
	Long findCustomerActiveAccount(@PathVariable("idCustomer") Long idCustomer,
			@PathVariable("idProduct") Long idProduct);

	/**
	 * Find all active accounts for customer.
	 *
	 * @author MoezMhiri
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-all-active-accounts-for-customer/{idCustomer}")
	List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(
			@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Load address by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/address/load-by-customer/{idCustomer}")
	List<AddressDTO> loadAddressByCustomer(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Load UDF by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/load-udf-by-customer/{idCustomer}")
	List<UserDefinedFieldsLinksDTO> loadUDFByCustomer(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Load UDF by loan (idAccountExtern).
	 * 
	 * @author HaythemBenizid
	 * @param idAccountExtern the id account extern
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/load-udf-by-loan/{idAccountExtern}")
	List<UserDefinedFieldsLinksDTO> loadUDFByLoan(
			@PathVariable("idAccountExtern") Long idAccountExtern);

	/**
	 * Reporting schedules status.
	 * 
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the list
	 */
	@PostMapping("/load-data-abacus/reporting-schedules-status")
	List<ReportingSchedulesStatusDTO> reportingSchedulesStatus(
			@RequestBody ReportingDTO reportingDTO);

	/**
	 * Find arrears details by ID Customer extern from ABACUS DB.
	 * 
	 * @author Salmen Fatnassi
	 * @param idCustomer the id Customer
	 * @return the arrears DTO
	 */
	@GetMapping("/load-data-abacus/customer-arrears/{idCustomer}")
	ArrearsDTO findArrearsByCustomer(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find all motif rejet.
	 * 
	 * @author ManelLamloum
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/loans/find-all-motif-rejet")
	List<SettingMotifRejetsDTO> findAllMotifRejet();

	/**
	 * Find canceled loan.
	 *
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/loans/find-canceled-loan")
	List<LoanDTO> findCanceledLoan();

	/**
	 * Find loan status issued.
	 * 
	 * @author Ines Dridi
	 * @param idLoanExtern the id loan extern
	 * @return the loan DTO
	 */
	@GetMapping("/load-data-abacus/loan-issued-by-id/{idLoanExtern}")
	List<LoanDTO> findLoanStatusIssued(@PathVariable("idLoanExtern") Long idLoanExtern);

	/**
	 * Find I score.
	 *
	 * @author YesserSomai
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/i-score/{startDate}/{endDate}")
	List<IScoreDTO> findIScore(@PathVariable("startDate") String startDate,
			@PathVariable("endDate") String endDate);

	/**
	 * Find active account for customer and loan.
	 *
	 * @author HaythemBenizid
	 * @param idAccounts the id accounts
	 * @return the long
	 */
	@PostMapping("/load-data-abacus/find-active-account-by-customer-loan")
	Long findActiveAccountForCustomerAndLoan(@RequestBody List<Long> idAccounts);

	/**
	 * Find customer paid account.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @param idProduct the id product
	 * @return the double
	 */
	@GetMapping("/load-data-abacus/find-customer-paid-account/{idCustomer}/{idProduct}")
	Double findCustomerPaidAccount(@PathVariable("idCustomer") Long idCustomer,
			@PathVariable("idProduct") Long idProduct);

	/**
	 * Load check paiment for given customer by customerNumber => return Map grouped by LoanID.
	 *
	 * @author HaythemBenizid
	 * @param customerNumber the customer number
	 * @return the map
	 */
	@GetMapping("/load-data-abacus/customer-check-paiment/{customerNumber}")
	Map<Long, List<ScheduleDTO>> loadCheckPaiment(
			@PathVariable("customerNumber") String customerNumber);

	/**
	 * Creates the expenses journal page.
	 * 
	 * @author idridi
	 * @param expensesJournalPageDTO the expenses journal page DTO
	 */
	@PostMapping("/load-data-api-abacus/create-journal-page")
	void createExpensesJournalPage(@RequestBody ExpensesJournalPageDTO expensesJournalPageDTO);

	/**
	 * Find account GL list.
	 *
	 * @author yesser.somai
	 * @param branchId the branch id
	 * @return the list of account GL
	 */
	@GetMapping("/load-data-abacus/find-account-list/{branchId}")
	List<String> findAccountGlList(@PathVariable("branchId") Long branchId);

	/**
	 * Find loans by given params used by batch.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @param limite the limite
	 * @return the list
	 * @throws Exception the exception
	 */
	@GetMapping("/load-data-abacus/loans/{limite}")
	List<LoanDTO> find(@RequestHeader("Authorization") String token,
			@PathVariable("limite") Long limite) throws Exception;

	/**
	 * Find loan by account number.
	 * 
	 * @author idridi
	 * @param accountNumber the account number
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/get-loan-by-accountNumber/{accountNumber}")
	List<LoanDTO> findLoanByAccountNumber(@PathVariable("accountNumber") String accountNumber);

	/**
	 * Find all users.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/users/{limite}")
	List<UserDTO> findUsers(@RequestHeader("Authorization") String token,
			@PathVariable("limite") Long limite);

	/**
	 * Find customer by given ID.
	 * 
	 * @author HaythemBenizid
	 * @param token the token
	 * @param idCustomer the id customer
	 * @return the customer DTO
	 */
	@GetMapping("/load-data-abacus/customer/{idCustomer}")
	CustomerDTO findCustomer(@RequestHeader("Authorization") String token,
			@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Find all Products from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/products/")
	List<ProductDTO> find(@RequestHeader("Authorization") String token);

	/**
	 * Load UDF for loan.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/load-udf-loan/{limite}")
	List<UserDefinedFieldsLinksDTO> loadUDFForLoan(@RequestHeader("Authorization") String token,
			@PathVariable("limite") Long limite);

	/**
	 * Load UDF for customer.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/load-udf-customer/{limite}")
	List<UserDefinedFieldsLinksDTO> loadUDFForCustomer(@RequestHeader("Authorization") String token,
			@PathVariable("limite") Long limite);

	/**
	 * Load address for customer.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/address/load-for-customer/{limite}")
	List<AddressDTO> loadAddressForCustomer(@RequestHeader("Authorization") String token,
			@PathVariable("limite") Long limite);

	/**
	 * Find issued loansby id externe.
	 * 
	 * @author idridi
	 * @param ids the ids
	 * @return the list
	 */
	@PostMapping("/load-data-abacus/find-loans-issued")
	List<LoanDTO> findIssuedLoansbyIdExterne(@RequestBody List<Long> ids);

	/**
	 * Find canceled loans.
	 *
	 * @author YesserSomai
	 * @param ids the ids
	 * @param token the token
	 * @return the list
	 */
	@PostMapping("/load-data-abacus/loans-Canceled")
	List<LoanDTO> findCanceledLoans(@RequestBody List<Long> ids,
			@RequestHeader("Authorization") String token);

	/**
	 * Find address list.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/address/find-address-list")
	List<AddressListDTO> findAddressList();

	/**
	 * Find address list value.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/address/find-address-list-value")
	List<AddressListValueDTO> findAddressListValue();

	/**
	 * Find address type.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/address/find-address-type")
	List<AddressTypeDTO> findAddressType();

	/**
	 * Find settings address.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/address/find-settings-address")
	List<AddressSettingAbacusDTO> findSettingsAddress();

	/**
	 * Find user defined fields.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/find-udf-fields")
	List<UserDefinedFieldsDTO> findUserDefinedFields();

	/**
	 * Find user defined field group.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/find-udf-group")
	List<UserDefinedFieldGroupDTO> findUserDefinedFieldGroup();

	/**
	 * Find user defined field list values.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/udf/find-udf-list-values")
	List<UserDefinedFieldListValuesDTO> findUserDefinedFieldListValues();

	/**
	 * Find branch.
	 * 
	 * @author HaythemBenizid
	 * @return the branche DTO
	 */
	@GetMapping("/load-data-abacus/setting/branche/")
	List<BrancheDTO> findBranches();

	/**
	 * Find product loan reasons.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("load-data-abacus/setting/find-product-loan-reasons/")
	List<ProductLoanReasonsDTO> findProductLoanReasons();

	/**
	 * Find loan guarantor source.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-loan-guarantor-source/")
	List<LoanGuarantorSourceDTO> findLoanGuarantorSource();

	/**
	 * Find loan source of funds.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-loan-source-of-funds/")
	List<LoanSourceOfFundsDTO> findLoanSourceOfFunds();

	/**
	 * Find loan refinance reason.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-loan-refinance-reason/")
	List<LoanRefinanceReasonDTO> findLoanRefinanceReason();

	/**
	 * Find relationship.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-relationship/")
	List<RelationshipDTO> findRelationship();

	/**
	 * Find industry.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-industry/")
	List<IndustryDTO> findIndustry();

	/**
	 * Find role ABACUS.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-role-abacus/")
	List<RoleAbacusDTO> findRoleAbacus();

	/**
	 * load all portfolio.
	 * 
	 * @author Salmen Fatnassi
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/portfolio")
	List<PortfolioDTO> findAllPortfolio();

	/**
	 * Send IScore request.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	@PostMapping("/api/run-iscore")
	ScreeningDTO sendIScoreRequest(@RequestBody ScreeningDTO screeningDTO);

	/**
	 * Generate I score report.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the byte[]
	 */
	@PostMapping("/api/report/iscore")
	byte[] generateIScoreReport(@RequestBody ScreeningDTO screeningDTO);

	/**
	 * Send request SOAP kyc get person details.
	 *
	 * @author yesser.somai
	 * @param kycDTO the kyc DTO
	 * @return the Kyc DTO
	 */
	@PostMapping("/get-person-details")
	KycDTO sendRequestSOAPKycGetPersonDetails(@RequestBody KycDTO kycDTO);

	/**
	 * Request SOAPI score and generate I score report.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	@PostMapping("/soap-api/soap-iscore-report")
	ScreeningDTO requestSOAPIScoreAndGenerateIScoreReport(@RequestBody ScreeningDTO screeningDTO);

	/**
	 * Find loans by given params used by button synchronize.
	 * 
	 * @author idridi
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/loans-in-abacus/{limite}")
	List<LoanDTO> find(@PathVariable("limite") Long limite);

	/**
	 * Find customer paid accounts.
	 * 
	 * @author idridi
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-customer-paid-accounts/{idCustomer}")
	List<Double> findCustomerPaidAccounts(@PathVariable("idCustomer") Long idCustomer);

	/**
	 * Creates the loan collateral.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the acm collateral DTO
	 */
	@PostMapping("/load-data-api-abacus/add-loan-collateral")
	List<AcmCollateralDTO> createLoanCollateral(@RequestBody LoanDTO loanDTO);

	/**
	 * Find collateral types.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-collateral-types/")
	List<CollateralTypeDTO> findCollateralTypes();

	/**
	 * Gets the closing balanceby id loan extern.
	 * 
	 * @author idridi
	 * @param idLoanExtern the id loan extern
	 * @return the closing balanceby id loan extern
	 */
	@GetMapping("/load-data-abacus/get-closing-balance-by-idLoan/{idLoanExtern}")
	Long getClosingBalancebyIdLoanExtern(@PathVariable("idLoanExtern") Long idLoanExtern);

	/**
	 * Creates the refinance loan.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 */
	@PutMapping("/load-data-api-abacus/create-refinance-loan")
	void createRefinanceLoan(@RequestBody LoanDTO loanDTO);

	/**
	 * Find branch change for batch.
	 * 
	 * @author mlamloum
	 * @param lastBranchChangeIdSynchronized the last branch change id synchronized
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-branch-change-list/{lastBranchChangeIdSynchronized}")
	List<BranchChangeDTO> findBranchChangeForBatch(
			@PathVariable("lastBranchChangeIdSynchronized") String lastBranchChangeIdSynchronized);

	/**
	 * Find cu account portfolio transferred.
	 *
	 * @param indexCuAccountPortfolioTransferred the index cu account portfolio transferred
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/find-account-portfolio-list/{indexCuAccountPortfolioTransferred}")
	List<CUAccountPortfolioTransferredDTO> findCuAccountPortfolioTransferred(
			@PathVariable("indexCuAccountPortfolioTransferred") Long indexCuAccountPortfolioTransferred);

	/**
	 * Find last cu account portfolio transferred id.
	 *
	 * @return the long
	 */
	@GetMapping("/load-data-abacus/get-cu-account-portfolio-transferred-last-id")
	Long findLastCuAccountPortfolioTransferredId();

	/**
	 * Gets the collection from abacus.
	 *
	 * @author idridi
	 * @param token the token
	 * @return the collection from abacus
	 */
	@GetMapping("/load-data-abacus/init-list-of-collection")
	Integer initCollectionAbacus(@RequestHeader("Authorization") String token);

	/**
	 * Gets the collection from abacus.
	 *
	 * @param token the token
	 * @param rowIndex the row index
	 * @return the collection from abacus
	 */
	@GetMapping("/load-data-abacus/get-list-of-collection/{rowIndex}")
	AcmCollectionDTO getCollectionFromAbacus(@RequestHeader("Authorization") String token,
			@PathVariable("rowIndex") Integer rowIndex);

	/**
	 * Find deferred period type.
	 * 
	 * @author mlamloum
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/setting/find-deferredPeriod-type-abacus/")
	List<DeferredPeriodTypeDTO> findDeferredPeriodType();

	/**
	 * Check setting topup validity.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the setting topup validity DTO
	 */
	@PostMapping("/load-data-abacus/setting-topup/check-validity")
	SettingTopupValidityDTO checkSettingTopupValidity(@RequestBody LoanDTO loanDTO);

	/**
	 * Creates the journal entry.
	 *
	 * @param journalEnteriesToAbacus the journal enteries to abacus
	 * @return the string
	 * @throws Exception the exception
	 */
	@PostMapping("/load-data-api-abacus/create-journal-entry")
	String createJournalEntry(@RequestBody JournalEnteriesToAbacusDTO journalEnteriesToAbacus)
			throws Exception;

	/**
	 * Find main account.
	 *
	 * @author kouali
	 * @param accountId the account id
	 * @param branchId the branch id
	 * @return the long
	 */
	@GetMapping("/load-data-abacus/find-main-account/{accountId}/{branchId}")
	Long findMainAccount(@PathVariable("accountId") Long accountId,
			@PathVariable("branchId") Integer branchId);

	/**
	 * Find account id.
	 *
	 * @param branchId the branch id
	 * @param number the number
	 * @return the long
	 */
	@PostMapping("/load-data-abacus/find-account-id/{branchId}")
	Long findAccountId(@PathVariable("branchId") Integer branchId, @RequestBody String number);

	/**
	 * Find fees.
	 *
	 * @return the list
	 */
	@GetMapping("/load-data-abacus/application-fees")
	List<ApplicationFeeDTO> findFees();

	/**
	 * Saudi by passport or nin.
	 *
	 * @param passportNo the passport no
	 * @param passportExpiryDate the passport expiry date
	 * @param nin the nin
	 * @param birthDateG the birth date G
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@GetMapping("/yakeen-api/saudiByPassportOrNin")
	ResponseEntity<String> saudiByPassportOrNin(
			@RequestParam(value = "passportNo", required = false) String passportNo,
			@RequestParam(value = "passportExpiryDate", required = false) String passportExpiryDate,
			@RequestParam(value = "nin", required = false) String nin,
			@RequestParam(value = "birthDateG", required = false) Date birthDateG)
			throws IOException;

	/**
	 * Send sms otp.
	 * 
	 * @param number the number
	 * @param msg the msg
	 * @return the response entity
	 */
	@GetMapping("/mobishastra-api/send-sms-otp")
	ResponseEntity<String> sendSmsOtp(@RequestParam(value = "number") String number,
			@RequestParam(value = "msg") String msg);

	/**
	 * Find target url and payment id.
	 *
	 * @param paymentApiSanadDTO the payment api sanad DTO
	 * @return the response entity
	 */
	@PostMapping("/payment-api/find-targetUrl-paymentId")
	ResponseEntity<String> findTargetUrlAndPaymentId(
			@RequestBody PaymentApiSanadDTO paymentApiSanadDTO);

	/**
	 * Disburse loan.
	 *
	 * @param disburseDto the disburse dto
	 * @return the disburse response
	 */
	@PostMapping("/load-data-api-abacus/disburse-loan")
	DisburseResponse disburseLoan(@RequestBody DisburseDTO disburseDto);

	/**
	 * Gets the cu fee id by id acount.
	 *
	 * @author kouali
	 * @param accountId the account id
	 * @return the cu fee id by id acount
	 */
	@GetMapping("/load-data-abacus/getIdFeeByIdAccount/{accountId}")
	Long getCuFeeIdByIdAcount(@PathVariable("accountId") Long accountId);

	/**
	 * Gets the score.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @return the score
	 */
	@PostMapping("/vneuron/calculRiskVneuron/{idLoan}/{customerId}")
	RiskResponse getScore(@RequestBody CustomerVneuron customerVneuron,
			@PathVariable("idLoan") Long idLoan, @PathVariable("customerId") Long customerId);

	/**
	 * Post kyc.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @return the risk response
	 */
	@PostMapping("/vneuron/postAML/{loanId}/{customerId}")
	SearchPersonCustomerResponse postAML(@RequestBody CustomerVneuron customerVneuron,
			@PathVariable("loanId") Long loanId, @PathVariable("customerId") Long customerId);

	/**
	 * Find account schedule by customer id.
	 *
	 * @param idCustomerExtern the id customer extern
	 * @param accountNumberExtern the account number extern
	 * @return the list
	 */
	@GetMapping("/load-data-api-abacus/customer-account-schedule/{idCustomerExtern}/{accountNumberExtern}")
	List<ScheduleDTO> findAccountScheduleByCustomerId(
			@PathVariable("idCustomerExtern") Long idCustomerExtern,
			@PathVariable("accountNumberExtern") String accountNumberExtern);

	/**
	 * Purchase.
	 *
	 * @param purchaseRequestDTO the purchase request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/murabha-api/purchase")
	ResponseEntity<PurchaseMurabhaApiResponseDTO> purchase(
			@RequestBody PurchaseMurabhaApiRequestDTO purchaseRequestDTO,
			@RequestParam(value = "loanId") Long loanId);

	/**
	 * Confirm purchase.
	 *
	 * @param confirmPurchaseRequestDTO the confirm purchase request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/murabha-api/confirm-purchase")
	ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmPurchase(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO confirmPurchaseRequestDTO,
			@RequestParam(value = "loanId") Long loanId);

	/**
	 * Sale.
	 *
	 * @param salePurchaseRequestDTO the sale purchase request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/murabha-api/sale")
	ResponseEntity<ConfirmPurchaseResponseApiDTO> sale(
			@RequestBody SaleMurabhaApiRequestDTO salePurchaseRequestDTO,
			@RequestParam(value = "loanId") Long loanId);

	/**
	 * Confirm sale.
	 *
	 * @param confirmSaleRequestDTO the confirm sale request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/murabha-api/confirm-sale")
	ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmSale(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO confirmSaleRequestDTO,
			@RequestParam(value = "loanId") Long loanId);

	/**
	 * Transfer notice.
	 *
	 * @param transferNotices the transfer notices
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/murabha-api/transfer-notice")
	ResponseEntity<ConfirmPurchaseResponseApiDTO> transferNotice(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO transferNotices,
			@RequestParam(value = "loanId") Long loanId);

	/**
	 * Initialize charge fee.
	 *
	 * @param accountId the account id
	 * @return the response charge fee DTO
	 */
	@GetMapping("/charge-fee/charge-fee-info/{accountId}")
	ResponseChargeFeeDTO initializeChargeFee(@PathVariable("accountId") Long accountId);

	/**
	 * Charge fee.
	 *
	 * @param chargeFeeDTO the charge fee DTO
	 * @return the response entity
	 */
	@PostMapping("/charge-fee/post-charge-fee")
	ResponseEntity<String> postChargeFees(@RequestBody ResponseChargeFeeDTO chargeFeeDTO);

	/**
	 * Emdha api.
	 *
	 * @param loanDTO the loan DTO
	 * @param token the token
	 * @return true, if successful
	 */
	@PostMapping("/emdha-api/sign")
	boolean emdhaApi(@RequestBody LoanDTO loanDTO, @RequestHeader("Authorization") String token);

	/**
	 * Gets the customer status.
	 *
	 * @param customerId the customer id
	 * @param idLoan the id loan
	 * @return the customer status
	 */
	@GetMapping("/vneuron/customer/status/{customerId}/{idLoan}")
	String getCustomerStatus(@PathVariable("customerId") Long customerId,
			@PathVariable("idLoan") Long idLoan);

	/**
	 * Gets the employment status.
	 *
	 * @param nationalId the national id
	 * @return the employment status
	 */
	@GetMapping("/dakhli-api/gosi/income/{nationalId}")
	ResponseEntity<ResponseIncomeDakhliApiDTO> getEmploymentStatus(@PathVariable String nationalId);

	/**
	 * Gets the score simah.
	 *
	 * @param request the request
	 * @return the score simah
	 */
	@PostMapping("/simah/score")
	ResponseEntity<String> getScoreSimah(RequestGetScoreDTO request);

	/**
	 * Enquiry new customer.
	 *
	 * @param request the request
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/simah/enquiry-new-customer/{loanId}")
	ResponseEntity<String> enquiryNewCustomer(
			@RequestBody RequestEnquiryNewCustomerSimahApiDTO request,
			@PathVariable("loanId") Long loanId);

	/**
	 * Gets the employment status.
	 *
	 * @param nationalId the national id
	 * @param loanId the loan id
	 * @return the employment status
	 */
	@GetMapping("/dakhli-api/gosi/income/{nationalId}/{loanId}")
	ResponseEntity<ResponseIncomeDakhliApiDTO> getEmploymentStatus(@PathVariable String nationalId,
			@PathVariable Long loanId);

	/**
	 * Mofeed api.
	 *
	 * @param identity the identity
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@GetMapping("/masder-api/mofeed/{identity}/{loanId}")
	ResponseEntity<EmploymentStatusInfoMasdrAPIDTO> mofeedApi(
			@PathVariable("identity") String identity, @PathVariable("loanId") Long loanId);
}
