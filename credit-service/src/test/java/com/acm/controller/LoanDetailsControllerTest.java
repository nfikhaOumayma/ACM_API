/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.collection.IsIterableContainingInAnyOrder.containsInAnyOrder;
import static org.junit.Assert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonFunctions;
import com.acm.service.LoanDetailsService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.FinancialAnalysisDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanDetailsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.models.transvers.LoanScheduleAPI;

/**
 * The class {@link LoanDetailsControllerTest}.
 *
 * @author MoezMhiri
 * @since 0.8.0
 */
class LoanDetailsControllerTest {

	/** The loan controller. */
	@InjectMocks
	private LoanDetailsController loanDetailsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan service. */
	@Mock
	private LoanDetailsService loanDetailsService;

	/** The transvers client. */
	@Mock
	private TransversClient transversClient;

	/** The parametrage client. */
	@Mock
	private ParametrageClient parametrageClient;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loanDetailsController).build();
	}

	/**
	 * Should success find details loan by id loan extern.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindDetailsLoan() throws Exception {

		// GIVEN
		LoanDetailsDTO loanDetailsDTO = new LoanDetailsDTO();
		given(loanDetailsService.findDetailsLoan(any(Long.class))).willReturn(loanDetailsDTO);

		// WHEN
		this.mockMvc
				.perform(
						get("/loans/data-abacus/loan-details/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanDetailsService, times(1)).findDetailsLoan(any(Long.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success find details customer by id loan extern.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindDetailsCustomer() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = new CustomerDTO();
		given(loanDetailsService.findDetailsCustomer(any(Long.class))).willReturn(customerDTO);

		// WHEN
		this.mockMvc.perform(
				get("/loans/data-abacus/customer-loan/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanDetailsService, times(1)).findDetailsCustomer(any(Long.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success find customer account schedule by id loan extern.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerAccountScheduleByLoan() throws Exception {

		// GIVEN
		ScheduleDTO scheduleDTO = new ScheduleDTO();
		given(loanDetailsService.findCustomerAccountScheduleByLoan(any(Long.class)))
				.willReturn(Collections.singletonList(scheduleDTO));

		// WHEN
		this.mockMvc
				.perform(get("/loans/data-abacus/customer-account-schedule/1")
						.content(CommonFunctions.toJson(scheduleDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanDetailsService, times(1)).findCustomerAccountScheduleByLoan(any(Long.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success find guarantors by id loan.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindGuarantors() throws Exception {

		// GIVEN
		GuarantorDTO guarantorDTO = new GuarantorDTO();
		given(loanDetailsService.findGuarantors(any(Long.class)))
				.willReturn(Collections.singletonList(guarantorDTO));

		// WHEN
		this.mockMvc
				.perform(get("/loans/data-abacus/guarantors/1")
						.content(CommonFunctions.toJson(guarantorDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanDetailsService, times(1)).findGuarantors(any(Long.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success find collaterals by id loan.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCollaterols() throws Exception {

		// GIVEN
		CollaterolDTO collaterolDTO = new CollaterolDTO();
		given(loanDetailsService.findCollaterols(any(Long.class)))
				.willReturn(Collections.singletonList(collaterolDTO));

		// WHEN
		this.mockMvc
				.perform(get("/loans/data-abacus/collaterols/1")
						.content(CommonFunctions.toJson(collaterolDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanDetailsService, times(1)).findCollaterols(any(Long.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success find financial analysis.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindFinancialAnalysis() throws Exception {

		// GIVEN
		FinancialAnalysisDTO financialAnalysisDTO = new FinancialAnalysisDTO();
		given(loanDetailsService.findFinancialAnalysis(any(Long.class)))
				.willReturn(Collections.singletonList(financialAnalysisDTO));

		// WHEN
		this.mockMvc
				.perform(get("/loans/data-abacus/financialanalysis/1")
						.content(CommonFunctions.toJson(financialAnalysisDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanDetailsService, times(1)).findFinancialAnalysis(any(Long.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success find required document.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindRequiredDocument() throws Exception {

		// GIVEN
		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		given(loanDetailsService.findRequiredDocument(any(LoanDTO.class)))
				.willReturn(Collections.singletonList(settingDocumentTypeDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans/findRequiredDocument")
						.content(CommonFunctions.toJson(settingDocumentTypeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanDetailsService, times(1)).findRequiredDocument(any(LoanDTO.class));
		verifyNoMoreInteractions(loanDetailsService);
	}

	/**
	 * Should success calculate loan schedules.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	@Disabled
	void shouldSuccessCalculateLoanSchedules() throws Exception {

		// GIVEN
		Long productId = 1L;
		ProductDTO productDTO = new ProductDTO();
		productDTO.setId(productId);
		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setLoanId(new Long(1));
		loanDTO.setProductDTO(productDTO);
		LoanScheduleAPI loanScheduleAPI = new LoanScheduleAPI();
		given(parametrageClient.findProductById(any(Long.class))).willReturn(productDTO);
		given(transversClient.calculateLoanSchedules(any(LoanDTO.class)))
				.willReturn(loanScheduleAPI);
		// WHEN
		this.mockMvc
				.perform(post("/loans/load-data-api-abacus/calculate-loan-schedules")
						.content(CommonFunctions.toJson(loanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(transversClient, times(1)).calculateLoanSchedules(any(LoanDTO.class));
		verifyNoMoreInteractions(transversClient);
		verify(parametrageClient, times(1)).findProductById(any(Long.class));
		verifyNoMoreInteractions(parametrageClient);
	}

	/**
	 * Should success find product by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindProductById() throws Exception {

		// GIVEN
		Long productId = 1L;
		// WHEN
		this.mockMvc.perform(get("/loans/data-abacus/product/1")
				.content(CommonFunctions.toJson(productId)).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
	}

	/**
	 * Should success find fee repayment.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindFeeRepayment() throws Exception {

		// GIVEN
		Long idAccount = 1L;
		// WHEN
		this.mockMvc.perform(get("/loans/load-data-abacus/fee-repayment/1")
				.content(CommonFunctions.toJson(idAccount)).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
	}

	/**
	 * Should success find application fee.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindApplicationFee() throws Exception {

		// GIVEN
		Long idAccount = 1L;
		// WHEN
		this.mockMvc.perform(get("/loans/load-data-abacus/application-fee/1")
				.content(CommonFunctions.toJson(idAccount)).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
	}

	/**
	 * Should success load category type.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessLoadCategoryType() throws Exception {

		// GIVEN
		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_LOAN_DATA).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_LOAN_DATA)
						.getValue()));
		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
						.getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.RISK).getValue()));

		acmStatutsDTOs.add(new AcmStatutsDTO(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getValue()));

		// WHEN
		this.mockMvc
				.perform(get("/loans/status").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		assertTrue(acmStatutsDTOs.size() == 19);
		// Test class property, and its value
		assertThat(acmStatutsDTOs, containsInAnyOrder(
				hasProperty("value", is(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_LOAN_DATA).getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4)
								.getValue())),
				hasProperty("value", is(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION).getValue())),
				hasProperty("value",
						is(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.SCREENING)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT)
								.getValue())),
				hasProperty("value",
						is(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.RISK)
								.getValue())),
				hasProperty("value", is(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getValue()))));
	}

	/**
	 * Should success load status ABACUS.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessLoadStatusABACUS() throws Exception {

		// GIVEN
		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(4, "Issued"));
		acmStatutsDTOs.add(new AcmStatutsDTO(8, "Charged off"));
		acmStatutsDTOs.add(new AcmStatutsDTO(16, "Bad debt"));
		acmStatutsDTOs.add(new AcmStatutsDTO(32, "Transferred"));
		acmStatutsDTOs.add(new AcmStatutsDTO(64, "Cancelled"));

		// WHEN
		this.mockMvc
				.perform(get("/loans/status-loan-abacus").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		assertTrue(acmStatutsDTOs.size() == 5);
		// Test class property, and its value
		assertThat(acmStatutsDTOs, containsInAnyOrder(hasProperty("value", is("Issued")),
				hasProperty("value", is("Charged off")), hasProperty("value", is("Bad debt")),
				hasProperty("value", is("Transferred")), hasProperty("value", is("Cancelled"))));
	}
}
