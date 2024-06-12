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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.MezaCardService;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.pagination.AcmMezaCardPaginationDTO;

/**
 * The Class {@link MezaCardControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class MezaCardControllerTest {

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;
	/** The meza card controller. */
	@InjectMocks
	private MezaCardController mezaCardController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The meza card service. */
	@Mock
	private MezaCardService mezaCardService;

	/**
	 * Sets the up.
	 * 
	 * @author ManelLamloum
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));

		this.mockMvc = MockMvcBuilders.standaloneSetup(mezaCardController).build();
	}

	/**
	 * Creates the acm meza card DTO.
	 *
	 * @author ManelLamloum
	 * @return the acm meza card DTO
	 */
	private AcmMezaCardDTO createAcmMezaCardDTO() {

		AcmMezaCardDTO acmMezaCardDTO = new AcmMezaCardDTO();
		acmMezaCardDTO.setIdMezaCard(new Long(1));
		return acmMezaCardDTO;
	}

	/**
	 * Should success upload meza card file.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	@Disabled
	void shouldSuccessUploadMezaCardFile() throws Exception {

		// GIVEN
		MockMultipartFile mockMultipartFile = new MockMultipartFile("files", "FileUploadTest.txt",
				"text/plain", "This is a Test".getBytes());
		MockMultipartFile mockMultipartFiles[] = new MockMultipartFile[1];
		mockMultipartFiles[0] = mockMultipartFile;
		// doNothing().when(mezaCardService).uploadFile((MultipartFile[])
		// any(Object.class),any(String.class),any(Boolean.class));

		// WHEN

		this.mockMvc.perform(
				post("/meza-card/upload-meza-card-file", CommonFunctions.toJson(mockMultipartFiles))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		// verify(mezaCardService, times(1)).uploadFile((MultipartFile[]) any(Object.class));
		verifyNoMoreInteractions(mezaCardService);

	}

	/**
	 * Should success find pagination.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindPagination() throws Exception {

		// GIVEN
		AcmMezaCardPaginationDTO acmMezaCardPaginationDTO = new AcmMezaCardPaginationDTO();
		given(mezaCardService.find(any(AcmMezaCardPaginationDTO.class)))
				.willReturn(acmMezaCardPaginationDTO);
		// WHEN
		this.mockMvc
				.perform(post("/meza-card/find-pagination")
						.content(CommonFunctions.toJson(acmMezaCardPaginationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(mezaCardService, times(1)).find(any(AcmMezaCardPaginationDTO.class));
		verifyNoMoreInteractions(mezaCardService);
	}

	/**
	 * Should success update cards.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessUpdateCards() throws Exception {

		// GIVEN
		List<AcmMezaCardDTO> acmMezaCardDTOs = new ArrayList<AcmMezaCardDTO>();
		AcmMezaCardDTO acmMezaCardDTO = new AcmMezaCardDTO();

		given(mezaCardService.save((acmMezaCardDTOs)))
				.willReturn(Collections.singletonList(acmMezaCardDTO));
		// WHEN
		this.mockMvc
				.perform(post("/meza-card/update-cards")
						.content(CommonFunctions.toJson(Collections.singletonList(acmMezaCardDTO)))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(mezaCardService, times(1)).save((List<AcmMezaCardDTO>) any(Object.class));
		verifyNoMoreInteractions(mezaCardService);
	}

	/**
	 * Should success find first order by card nubmer.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindFirstOrderByCardNubmer() throws Exception {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = new AcmMezaCardDTO();
		given(mezaCardService.findByBranchIDAndStatus(acmMezaCardDTO))
				.willReturn(any(AcmMezaCardDTO.class));

		// WHEN
		this.mockMvc
				.perform(post("/meza-card/find-first-order-by-cardNumber")
						.content(CommonFunctions.toJson(acmMezaCardDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(mezaCardService, times(1)).findByBranchIDAndStatus(any(AcmMezaCardDTO.class));
		verifyNoMoreInteractions(mezaCardService);
	}

	/**
	 * Should success update meza card.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateMezaCard() throws Exception {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = createAcmMezaCardDTO();
		given(mezaCardService.save(acmMezaCardDTO.getIdMezaCard(), acmMezaCardDTO))
				.willReturn(acmMezaCardDTO);

		// WHEN
		this.mockMvc
				.perform(put("/meza-card/update").content(CommonFunctions.toJson(acmMezaCardDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(mezaCardService, times(1)).save(any(Long.class), any(AcmMezaCardDTO.class));
		verifyNoMoreInteractions(mezaCardService);
	}

	/**
	 * Should success update status customer.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateStatusCustomer() throws Exception {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = createAcmMezaCardDTO();
		given(mezaCardService.update(any(AcmMezaCardDTO.class))).willReturn(acmMezaCardDTO);

		// WHEN
		this.mockMvc
				.perform(post("/meza-card/update-status-customer")
						.content(CommonFunctions.toJson(acmMezaCardDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(mezaCardService, times(1)).update(any(AcmMezaCardDTO.class));
		verifyNoMoreInteractions(mezaCardService);
	}

	/**
	 * Should success return list meza card.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReturnListMezaCard() throws Exception {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = new AcmMezaCardDTO();
		given(mezaCardService.find(any(AcmMezaCardDTO.class)))
				.willReturn(Collections.singletonList(acmMezaCardDTO));
		// WHEN
		this.mockMvc
				.perform(post("/meza-card/").content(CommonFunctions.toJson(acmMezaCardDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(mezaCardService, times(1)).find(any(AcmMezaCardDTO.class));
		verifyNoMoreInteractions(mezaCardService);
	}

	/**
	 * Should success generate meza card report.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessGenerateMezaCardReport() throws Exception {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = new AcmMezaCardDTO();
		byte[] data = {1, 2, 3, 4, 5};
		given(mezaCardService.downloadReport(any(AcmMezaCardDTO.class))).willReturn(data);
		// WHEN
		this.mockMvc
				.perform(post("/meza-card/report-meza-card")
						.content(CommonFunctions.toJson(acmMezaCardDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(mezaCardService, times(1)).downloadReport(any(AcmMezaCardDTO.class));
		verifyNoMoreInteractions(mezaCardService);
	}
}
