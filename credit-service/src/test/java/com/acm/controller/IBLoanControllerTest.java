/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
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

import com.acm.constants.common.CommonFunctions;
import com.acm.service.IBLoanService;
import com.acm.utils.dtos.IBLoanDTO;
import com.acm.utils.dtos.LoanStatutDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;

/**
 * The class {@link IBLoanControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IBLoanControllerTest {

	/** The ib loan controller. */
	@InjectMocks
	private IBLoanController ibLoanController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan ib service. */
	@Mock
	private IBLoanService loanIbService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(ibLoanController).build();
	}

	/**
	 * Creates the IB loan DTO.
	 *
	 * @author HaythemBenizid
	 * @return the iBLoan DTO
	 */
	private IBLoanDTO createIBLoanDTO() {

		IBLoanDTO iBLoanDTO = new IBLoanDTO();
		iBLoanDTO.setId(new Long(1));
		return iBLoanDTO;
	}

	/**
	 * Should success find iBLoan by id.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIBLoanById() throws Exception {

		// GIVEN
		IBLoanDTO iBLoanDTO = createIBLoanDTO();
		given(loanIbService.find(any(Long.class))).willReturn(iBLoanDTO);

		// WHEN
		this.mockMvc.perform(get("/loans-ib/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanIbService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should return list iBLoan DTO.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListIBLoanDTO() throws Exception {

		// GIVEN
		IBLoanDTO iBLoanDTO = new IBLoanDTO();
		given(loanIbService.find(any(IBLoanDTO.class)))
				.willReturn(Collections.singletonList(iBLoanDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans-ib/").content(CommonFunctions.toJson(iBLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanIbService, times(1)).find(any(IBLoanDTO.class));
		verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should success save iBLoan.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveIBLoan() throws Exception {

		// // WHEN
		// given(loanIbService.save(any(IBLoanDTO.class))).willReturn(new IBLoanDTO());
		//
		// // WHEN
		// this.mockMvc
		// .perform(post("/loans-ib/create").content(CommonFunctions.toJson(new IBLoanDTO()))
		// .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
		// .andExpect(status().isOk());
		// // THEN
		// verify(loanIbService, times(1)).save(any(IBLoanDTO.class));
		// verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should success find documents pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIBPagination() throws Exception {

		// GIVEN
		given(loanIbService.find(any(IBLoanPaginationDTO.class)))
				.willReturn(new IBLoanPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans-ib/find-ib-pagination")
						.content(CommonFunctions.toJson(new IBLoanPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanIbService, times(1)).find(any(IBLoanPaginationDTO.class));
		verifyNoMoreInteractions(loanIbService);
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
		IBLoanDTO iBLoanDTO = createIBLoanDTO();
		given(loanIbService.save(iBLoanDTO.getId(), iBLoanDTO)).willReturn(iBLoanDTO);

		// WHEN
		this.mockMvc
				.perform(put("/loans-ib/update").content(CommonFunctions.toJson(iBLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanIbService, times(1)).save(any(Long.class), any(IBLoanDTO.class));
		verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should success assigned all.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessAssignedAll() throws Exception {

		// GIVEN
		IBLoanDTO iBLoanDTO = createIBLoanDTO();
		List<IBLoanDTO> ibLoanDTOs = new ArrayList<>();
		ibLoanDTOs.add(iBLoanDTO);
		given(loanIbService.assignedAll(ibLoanDTOs)).willReturn(ibLoanDTOs);

		// WHEN
		this.mockMvc
				.perform(put("/loans-ib/assigned-all").content(CommonFunctions.toJson(ibLoanDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanIbService, times(1)).assignedAll((List<IBLoanDTO>) any(Object.class));
		verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should load filter product.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldLoadFilterProduct() throws Exception {

		// GIVEN
		IBLoanDTO iBLoanDTO = new IBLoanDTO();
		given(loanIbService.loadFilterProduct(any(IBLoanDTO.class)))
				.willReturn(Collections.singletonList(iBLoanDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loans-ib/load-filter-product-ib")
						.content(CommonFunctions.toJson(iBLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanIbService, times(1)).loadFilterProduct(any(IBLoanDTO.class));
		verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should success accept.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessAccept() throws Exception {

		// GIVEN
		IBLoanDTO iBLoanDTO = createIBLoanDTO();
		given(loanIbService.accept(iBLoanDTO.getId(), iBLoanDTO)).willReturn(iBLoanDTO);

		// WHEN
		this.mockMvc
				.perform(put("/loans-ib/accept").content(CommonFunctions.toJson(iBLoanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanIbService, times(1)).accept(any(Long.class), any(IBLoanDTO.class));
		verifyNoMoreInteractions(loanIbService);
	}

	/**
	 * Should return count.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnCount() throws Exception {

		// GIVEN
		LoanStatutDTO loanStatutDTO = new LoanStatutDTO();
		given(loanIbService.count()).willReturn(loanStatutDTO);

		// WHEN
		this.mockMvc.perform(get("/loans-ib/count").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanIbService, times(1)).count();
		verifyNoMoreInteractions(loanIbService);
	}
}
