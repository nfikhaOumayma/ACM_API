/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonFunctions;
import com.acm.service.LoanService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.pagination.LoanPaginationDTO;

/**
 * The class {@link LoanControllerTest}.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class LoanControllerTest {

	/** The loan controller. */
	@InjectMocks
	private LoanController loanController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan service. */
	@Mock
	private LoanService loanService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loanController).build();
	}

	/**
	 * Creates the loan DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the loan DTO
	 */
	private LoanDTO createLoanDTO() {

		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setLoanId(new Long(1));
		return loanDTO;
	}

	/**
	 * Should success find loan by id.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanById() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		given(loanService.find(any(Long.class))).willReturn(loanDTO);

		// WHEN
		this.mockMvc.perform(get("/loans/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should return count LoanStatut DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnCountLoanStatutDTO() throws Exception {

		// GIVEN
		given(loanService.count(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED)).willReturn(5L);

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-approved").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED);
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should return list loan DTO.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListLoanDTO() throws Exception {

		// GIVEN
		LoanDTO loanDTO = new LoanDTO();
		given(loanService.find(any(LoanDTO.class))).willReturn(Collections.singletonList(loanDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans/").content(CommonFunctions.toJson(loanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanService, times(1)).find(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success update loan.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateLoan() throws Exception {

		// GIVEN
		given(loanService.save(any(Long.class), any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(put("/loans/update").content(CommonFunctions.toJson(createLoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).save(any(Long.class), any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success save loan.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveLoan() throws Exception {

		// GIVEN
		given(loanService.save(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/create").content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).save(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success find loan.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteLoan() throws Exception {

		// GIVEN
		doNothing().when(loanService).delete(any(LoanDTO.class));

		// WHEN
		this.mockMvc.perform(delete("/loans/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService).delete(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success Rejected loan.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessRejectedLoan() throws Exception {

		// GIVEN
		given(loanService.rejected(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/rejected").content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).rejected(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success cancelled loan.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCancelledLoan() throws Exception {

		// GIVEN
		given(loanService.cancelled(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/cancelled").content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).cancelled(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success validate loan.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessValidateLoan() throws Exception {

		// GIVEN
		given(loanService.validate(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/validate").content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).validate(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success find pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindPagination() throws Exception {

		// GIVEN
		given(loanService.find(any(LoanPaginationDTO.class))).willReturn(new LoanPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/find-pagination")
						.content(CommonFunctions.toJson(new LoanPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).find(any(LoanPaginationDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success find un assignment pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUnAssignmentPagination() throws Exception {

		// GIVEN
		given(loanService.findUnAssignment(any(LoanPaginationDTO.class)))
				.willReturn(new LoanPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/find-unassigned-pagination")
						.content(CommonFunctions.toJson(new LoanPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).findUnAssignment(any(LoanPaginationDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success validate issued by batch.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessValidateIssuedByBatch() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.validateIssuedByBatch(loanDTOs)).willReturn(loanDTOs);

		// WHEN
		this.mockMvc.perform(post("/loans/validate-by-batch")
				.content(CommonFunctions.toJson(loanDTOs)).header("Authorization", "TOKEN")
				.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).validateIssuedByBatch((List<LoanDTO>) any(Object.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success validate ready for disbursement.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessValidateReadyForDisbursement() throws Exception {

		// GIVEN
		given(loanService.validateReadyForDisbursement(any(LoanDTO.class)))
				.willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/validate-for-disbusrsement")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).validateReadyForDisbursement(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success load filter status workflow.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadFilterStatusWorkflow() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.loadFilterStatusWorkflow(any(LoanDTO.class))).willReturn(loanDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/loans/load-filter-status-workflow")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).loadFilterStatusWorkflow(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success load filter product.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadFilterProduct() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.loadFilterProduct(any(LoanDTO.class))).willReturn(loanDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/loans/load-filter-product")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).loadFilterProduct(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success reassigned.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReassigned() throws Exception {

		// GIVEN
		given(loanService.reassigned(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/reassigned").content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).reassigned(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success validate all.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessValidateAll() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.validateAll(loanDTOs)).willReturn(loanDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/loans/validate-all").content(CommonFunctions.toJson(loanDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).validateAll((List<LoanDTO>) any(Object.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success save to abacus.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveToAbacus() throws Exception {

		// GIVEN
		given(loanService.saveToAbacus(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans/create-to-abacus")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).saveToAbacus(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success update for application.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateForApplication() throws Exception {

		// GIVEN
		given(loanService.updateForApplication(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(put("/loans/update-loan").content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).updateForApplication(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success assign to customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessAssignToCustomer() throws Exception {

		// GIVEN
		given(loanService.assignToCustomer(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(put("/loans/update-assign-customer")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).assignToCustomer(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success save loan group to abacus.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessSaveLoanGroupToAbacus() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.saveLoanGroupToAbacus(loanDTOs)).willReturn(loanDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/loans/create-loan-grp-to-abacus")
						.content(CommonFunctions.toJson(loanDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).saveLoanGroupToAbacus((List<LoanDTO>) any(Object.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success update group application.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessUpdateGroupApplication() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.updateGroupApplication(loanDTOs)).willReturn(loanDTO);

		// WHEN
		this.mockMvc
				.perform(put("/loans/update-loan-group").content(CommonFunctions.toJson(loanDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).updateGroupApplication((List<LoanDTO>) any(Object.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success check loan status issued.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCheckLoanStatusIssued() throws Exception {

		// GIVEN
		given(loanService.checkLoanStatusIssued(any(Long.class))).willReturn(Boolean.TRUE);

		// WHEN
		this.mockMvc.perform(get("/loans/check-issued-status/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).checkLoanStatusIssued(any(Long.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success update status.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateStatus() throws Exception {

		// GIVEN
		given(loanService.updateStatus(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(put("/loans/update-status")
						.content(CommonFunctions.toJson(createLoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).updateStatus(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count my task.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountMyTask() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-my-task").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab new.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabNew() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-new").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab drafts.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabDrafts() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-drafts").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab pending approval.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabPendingApproval() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc
				.perform(get("/loans/count-tab-pendingApproval").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab approved.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabApproved() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-approved").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab rejected.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabRejected() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-rejected").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab cancelled.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabCancelled() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-cancelled").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab reviewd.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabReviewd() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-review").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count tab issued.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountTabIssued() throws Exception {

		// GIVEN
		given(loanService.count(any(String.class))).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-issued").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).count(any(String.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * Should success count unassigned loans.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountUnassignedLoans() throws Exception {

		// GIVEN

		// WHEN
		this.mockMvc.perform(get("/loans/count-tab-unassigned").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
	}

	/**
	 * @author HaythemBenizid
	 * @throws Exception
	 */
	@Test
	void shouldSuccessAssignLoan() throws Exception {

		// GIVEN
		given(loanService.assignLoan(any(LoanDTO.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc
				.perform(put("/loans/assign-loan").content(CommonFunctions.toJson(createLoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).assignLoan(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * @author HaythemBenizid
	 * @throws Exception
	 */
	@Test
	void shouldSuccessLoadFilterProductForUnassignedLoans() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.loadFilterProductForUnassignedLoans(any(LoanDTO.class)))
				.willReturn(loanDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/loans/load-filter-product-loans_unassigned")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).loadFilterProductForUnassignedLoans(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}

	/**
	 * @author HaythemBenizid
	 * @throws Exception
	 */
	@Test
	void shouldSuccessLoadFilterBranch() throws Exception {

		// GIVEN
		LoanDTO loanDTO = createLoanDTO();
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loanDTOs.add(loanDTO);
		given(loanService.loadFilterBranch(any(LoanDTO.class))).willReturn(loanDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/loans/load-filter-branch")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanService, times(1)).loadFilterBranch(any(LoanDTO.class));
		verifyNoMoreInteractions(loanService);
	}
}
