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

import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.AcmDocumentsService;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.LoansDocumentsDTO;
import com.acm.utils.dtos.pagination.AcmDocumentsPaginationDTO;

/**
 * The class {@link AcmDocumentsControllerTest}.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public class AcmDocumentsControllerTest {

	/** The documentsLoan controller. */
	@InjectMocks
	private AcmDocumentsController documentsLoanController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The documentsLoan service. */
	@Mock
	private AcmDocumentsService documentsLoanService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(documentsLoanController).build();
	}

	/**
	 * Creates the documentsLoan DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the documentsLoan DTO
	 */
	private AcmDocumentsDTO createDocumentsLoanDTO() {

		AcmDocumentsDTO documentsLoanDTO = new AcmDocumentsDTO();
		documentsLoanDTO.setIdDocument(new Long(1));
		return documentsLoanDTO;
	}

	/**
	 * Should success find documentsLoan by id.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindDocumentsLoanById() throws Exception {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = createDocumentsLoanDTO();
		given(documentsLoanService.find(any(Long.class))).willReturn(documentsLoanDTO);

		// WHEN
		this.mockMvc.perform(get("/loans-documents/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentsLoanService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should return list documentsLoan DTO.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListDocumentsLoanDTO() throws Exception {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = new AcmDocumentsDTO();
		given(documentsLoanService.find(any(AcmDocumentsDTO.class)))
				.willReturn(Collections.singletonList(documentsLoanDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans-documents/").content(CommonFunctions.toJson(documentsLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(documentsLoanService, times(1)).find(any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should success save documentsLoan.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveDocumentsLoan() throws Exception {

		// WHEN
		given(documentsLoanService.save(any(AcmDocumentsDTO.class)))
				.willReturn(new AcmDocumentsDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans-documents/create")
						.content(CommonFunctions.toJson(new AcmDocumentsDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentsLoanService, times(1)).save(any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should return list find loans documents by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListFindLoansDocumentsByCustomer() throws Exception {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = new AcmDocumentsDTO();
		LoansDocumentsDTO loansDocumentsDTO = new LoansDocumentsDTO();
		given(documentsLoanService.findLoansDocumentsByCustomer(any(AcmDocumentsDTO.class)))
				.willReturn(Collections.singletonList(loansDocumentsDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans-documents/find-documents-customer")
						.content(CommonFunctions.toJson(documentsLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(documentsLoanService, times(1))
				.findLoansDocumentsByCustomer(any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should success find documents pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindDocumentsPagination() throws Exception {

		// GIVEN
		given(documentsLoanService.find(any(AcmDocumentsPaginationDTO.class)))
				.willReturn(new AcmDocumentsPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans-documents/find-pagination")
						.content(CommonFunctions.toJson(new AcmDocumentsPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentsLoanService, times(1)).find(any(AcmDocumentsPaginationDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should success update.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		AcmDocumentsDTO acmDocumentsDTO = createDocumentsLoanDTO();
		given(documentsLoanService.save(acmDocumentsDTO.getIdDocument(), acmDocumentsDTO))
				.willReturn(acmDocumentsDTO);

		// WHEN
		this.mockMvc
				.perform(put("/loans-documents/update")
						.content(CommonFunctions.toJson(acmDocumentsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentsLoanService, times(1)).save(any(Long.class), any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should success delete documents.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteDocuments() throws Exception {

		// GIVEN
		doNothing().when(documentsLoanService).delete(any(AcmDocumentsDTO.class));

		// WHEN
		this.mockMvc.perform(delete("/loans-documents/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentsLoanService).delete(any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should success disable document.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDisableDocument() throws Exception {

		// GIVEN
		AcmDocumentsDTO acmDocumentsDTO = createDocumentsLoanDTO();
		// WHEN
		this.mockMvc
				.perform(put("/loans-documents/disable-document")
						.content(CommonFunctions.toJson(acmDocumentsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentsLoanService, times(1)).disableDocument(any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}

	/**
	 * Should return find expenses document.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindExpensesDocument() throws Exception {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = new AcmDocumentsDTO();
		given(documentsLoanService.findExpensesDocument(any(AcmDocumentsDTO.class)))
				.willReturn(Collections.singletonList(documentsLoanDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans-documents/find-expenses-document")
						.content(CommonFunctions.toJson(documentsLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(documentsLoanService, times(1)).findExpensesDocument(any(AcmDocumentsDTO.class));
		verifyNoMoreInteractions(documentsLoanService);
	}
}
