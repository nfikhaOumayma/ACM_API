/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller.api_ib;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.client.ReportingClient;
import com.acm.service.api_ib.LoadDataIBService;
import com.acm.utils.dtos.AcmClaimsDTO;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.ClaimsPaginationDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;

/**
 * The Class LoadDataIBController.
 */
@RestController
@RequestMapping("/load-data-ib")
public class LoadDataIBController {

	/** The load data IB service. */
	@Autowired
	private LoadDataIBService loadDataIBService;

	/** The reporting client. */
	@Autowired
	private ReportingClient reportingClient;

	/**
	 * Find pagination.
	 *
	 * @param loanIbPaginationDTO the loan ib pagination DTO
	 * @return the IB loan pagination DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/find-pagination")
	public IBLoanPaginationDTO findPagination(@RequestBody IBLoanPaginationDTO loanIbPaginationDTO)
			throws IOException {

		return loadDataIBService.find(loanIbPaginationDTO);
	}

	/**
	 * Find udf link.
	 *
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the list
	 */
	@PostMapping("/udf-links/find-udf-groupby")
	public List<UDFLinksGroupeFieldsDTO> findUdfLink(
			@RequestBody UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		return loadDataIBService.findUdfLink(userDefinedFieldsLinksDTO);
	}

	/**
	 * Find.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	@PostMapping("/customers")
	public List<CustomerDTO> find(@RequestBody CustomerDTO customerDTO) {

		return loadDataIBService.find(customerDTO);
	}

	/**
	 * Find.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	@PostMapping("/loans")
	public List<LoanDTO> find(@RequestBody LoanDTO loanDTO) {

		return loadDataIBService.find(loanDTO);
	}

	/**
	 * Update loan.
	 *
	 * @param loanDTO the loan DTO
	 */
	@PostMapping("/loans/update")
	public void updateLoan(@RequestBody LoanDTO loanDTO) {

		loadDataIBService.updateAcmLoanInIB(loanDTO);
	}

	/**
	 * Find all customer information.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	@PostMapping("/customers/get-all-customer-information")
	public List<CustomerDTO> findAllCustomerInformation(@RequestBody CustomerDTO customerDTO) {

		return loadDataIBService.findAllCustomerInformation(customerDTO);
	}

	/**
	 * Update acm loan and customer in IB.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	@PostMapping("/update-loan-and-customer")
	public LoanDTO updateAcmLoanAndCustomerInIB(@RequestBody LoanDTO loanDTO) {

		return loadDataIBService.updateAcmLoanAndCustomerInIB(loanDTO);
	}

	/**
	 * Gets the documents from ib.
	 *
	 * @param documentsLoanDTO the documents loan DTO
	 * @param idAcmLoan the id acm loan
	 * @return the documents from ib
	 */
	@PostMapping("/get-document-and-save-in-ACM/{acmLoanId}")
	public List<AcmDocumentsDTO> getDocumentsFromIbAndSaveInACM(
			@RequestBody AcmDocumentsDTO documentsLoanDTO,
			@PathVariable("acmLoanId") Long idAcmLoan) {

		return loadDataIBService.getDocumentsFromIbAndSaveInAcm(documentsLoanDTO, idAcmLoan);
	}

	/**
	 * Display document.
	 *
	 * @param id the id
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@GetMapping(path = "/documents/display/")
	public byte[] displayDocument(@RequestParam("id") String id,
			@RequestParam("loanId") String loanId) {

		return loadDataIBService.displayDocument(id, loanId);
	}

	/**
	 * Save loan schedule in ib.
	 *
	 * @param loanScheduleDTO the loan schedule DTO
	 * @return the loan schedule DTO
	 */
	@PostMapping("/loan/schedules")
	public LoanScheduleDTO saveLoanScheduleInIb(@RequestBody LoanScheduleDTO loanScheduleDTO) {

		return loadDataIBService.saveLoanScheduleInIb(loanScheduleDTO);
	}

	/**
	 * Find claims.
	 *
	 * @param acmClaimsDTO the acm claims DTO
	 * @return the list
	 */
	@PostMapping("/claims")
	public List<AcmClaimsDTO> findClaims(@RequestBody AcmClaimsDTO acmClaimsDTO) {

		return loadDataIBService.findClaims(acmClaimsDTO);
	}

	/**
	 * Update claims.
	 *
	 * @param acmClaimsDTO the acm claims DTO
	 * @return the acm claims DTO
	 */
	@PostMapping("/claims/update")
	public AcmClaimsDTO updateClaims(@RequestBody AcmClaimsDTO acmClaimsDTO) {

		return loadDataIBService.updateClaims(acmClaimsDTO);
	}

	/**
	 * Find claims pagination.
	 *
	 * @param claimsPaginationDTO the claims pagination DTO
	 * @return the claims pagination DTO
	 */
	@PostMapping("/claims/find-pagination")
	public ClaimsPaginationDTO findClaimsPagination(
			@RequestBody ClaimsPaginationDTO claimsPaginationDTO) {

		return loadDataIBService.findClaimsPagination(claimsPaginationDTO);
	}

	/**
	 * Login api ib.
	 *
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@GetMapping("/ib-token")
	public String loginApiIb() throws IOException, URISyntaxException {

		return loadDataIBService.loginApiIb();
	}

	/**
	 * Reset setting from ACM.
	 *
	 * @param productsACM the products ACM
	 * @return the string
	 */
	@PostMapping("/reload-setting")
	public String resetSettingFromACM(@RequestBody List<ProductDTO> productsACM) {

		return loadDataIBService.SyncSettingFromACM(productsACM);
	}

}
