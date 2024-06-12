/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.api_ib;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import com.acm.utils.dtos.AcmClaimsDTO;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.IBLoanDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.ClaimsPaginationDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;

/**
 * {@link LoadDataIBService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface LoadDataIBService {

	/**
	 * Find.
	 *
	 * @param id the id
	 * @return the IB loan DTO
	 */
	IBLoanDTO find(Long id);

	/**
	 * Find.
	 *
	 * @param ibLoanPaginationDTO the ib loan pagination DTO
	 * @return the IB loan pagination DTO
	 */
	IBLoanPaginationDTO find(IBLoanPaginationDTO ibLoanPaginationDTO);

	/**
	 * Find customer by id.
	 *
	 * @param customerId the customer id
	 * @return the customer DTO
	 */
	CustomerDTO findCustomerById(Long customerId);

	/**
	 * Save.
	 * 
	 * @param ibLoanDTO the ib loan DTO
	 * @return the IB loan DTO
	 */
	IBLoanDTO update(IBLoanDTO ibLoanDTO);

	/**
	 * Update.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 */
	CustomerDTO update(CustomerDTO customerDTO);

	/**
	 * Save.
	 *
	 * @param loanDTO the loan DTO
	 * @return the IB loan DTO
	 */
	LoanDTO saveAcmLoanInIB(LoanDTO loanDTO);

	/**
	 * Update acm loan in IB.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	LoanDTO updateAcmLoanAndCustomerInIB(LoanDTO loanDTO);

	/**
	 * Update acm loan in IB.
	 *
	 * @param loanDTO the loan DTO
	 */
	void updateAcmLoanInIB(LoanDTO loanDTO);

	/**
	 * Update gurantors.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	LoanDTO updateGurantors(LoanDTO loanDTO);

	/**
	 * Creates the user.
	 *
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	UserDTO createUser(UserDTO userDTO);

	/**
	 * Find udf link.
	 *
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the list
	 */
	List<UDFLinksGroupeFieldsDTO> findUdfLink(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO);

	/**
	 * Find.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	List<CustomerDTO> find(CustomerDTO customerDTO);

	/**
	 * Find.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<LoanDTO> find(LoanDTO loanDTO);

	/**
	 * Find address.
	 *
	 * @param addressDTO the address DTO
	 * @return the list
	 */
	List<AddressDTO> findAddress(AddressDTO addressDTO);

	/**
	 * Find all customer information from ib : udf links and address.
	 *
	 * @author mlamloum
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	List<CustomerDTO> findAllCustomerInformation(CustomerDTO customerDTO);

	/**
	 * Find claims.
	 *
	 * @param acmClaimsDTO the acm claims DTO
	 * @return the list
	 */
	List<AcmClaimsDTO> findClaims(AcmClaimsDTO acmClaimsDTO);

	/**
	 * Save loan schedule in ib.
	 *
	 * @param loanScheduleDTO the loan schedule DTO
	 * @return the loan schedule DTO
	 */
	LoanScheduleDTO saveLoanScheduleInIb(LoanScheduleDTO loanScheduleDTO);

	/**
	 * Save to ged.
	 *
	 * @param uploadedFiles the uploaded files
	 * @param documentsLoanDTOs the documents loan DT os
	 */
	void saveToGed(byte[] uploadedFiles, List<AcmDocumentsDTO> documentsLoanDTOs);

	/**
	 * Gets the documents from ib.
	 * 
	 * @author mlamloum
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the documents from ib
	 */
	List<AcmDocumentsDTO> getDocumentsFromIb(AcmDocumentsDTO documentsLoanDTO);

	/**
	 * Display document.
	 *
	 * @author mlamloum
	 * @param id the id
	 * @param loanId the loan id
	 * @return the response entity
	 */
	byte[] displayDocument(String id, String loanId);

	/**
	 * Gets the documents from ib and save in acm.
	 *
	 * @param documentsLoanDTO the documents loan DTO
	 * @param idAcmLoan the id acm loan
	 * @return the documents from ib and save in acm
	 */
	List<AcmDocumentsDTO> getDocumentsFromIbAndSaveInAcm(AcmDocumentsDTO documentsLoanDTO,
			Long idAcmLoan);

	/**
	 * Update claims.
	 *
	 * @param acmClaimsDTO the acm claims DTO
	 * @return the acm claims DTO
	 */
	AcmClaimsDTO updateClaims(AcmClaimsDTO acmClaimsDTO);

	/**
	 * Login api ib.
	 *
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	String loginApiIb() throws IOException, URISyntaxException;

	/**
	 * Find guarantors.
	 *
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	List<CustomerLinksRelationshipDTO> findGuarantors(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO);

	/**
	 * Sync setting from ACM.
	 *
	 * @param productsACM the products ACM
	 * @return the string
	 */
	String SyncSettingFromACM(List<ProductDTO> productsACM);

	/**
	 * Creates the single sanad el amer.
	 *
	 * @param loanIbId the loan ib id
	 * @return true, if successful
	 */
	boolean createSingleSanadElAmer(Long loanIbId);

	/**
	 * Find claims pagination.
	 *
	 * @param claimsPaginationDTO the claims pagination DTO
	 * @return the claims pagination DTO
	 */
	ClaimsPaginationDTO findClaimsPagination(ClaimsPaginationDTO claimsPaginationDTO);
}
