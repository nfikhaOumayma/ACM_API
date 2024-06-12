/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ReportVisitRepository;
import com.acm.service.impl.ReportVisitServiceImpl;
import com.acm.utils.dtos.ReportVisitDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.ReportVisit;

/**
 * {@link ReportVisitServiceTest} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
@RunWith(SpringRunner.class)
class ReportVisitServiceTest {

	/** The acmReportVisit service. */
	@InjectMocks
	private ReportVisitServiceImpl acmReportVisitService;

	/** The acmReportVisit repository. */
	@Mock
	private ReportVisitRepository acmReportVisitRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Initialize a new UserDTO.
	 * 
	 * @return a new UserDTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin("login");
		userDTO.setPrenom("benTest");
		return userDTO;
	}

	/**
	 * Should success find acmReportVisit.
	 * 
	 * @author YesserSomai
	 */
	@Test
	void shouldSuccessFindAcmReportVisit() {

		// GIVEN
		ReportVisit acmReportVisit = initAcmReportVisit();
		ReportVisitDTO acmReportVisitDTO = initAcmReportVisitDTO();
		given(acmReportVisitRepository.findAll())
				.willReturn(Collections.singletonList(acmReportVisit));
		given(mapper.map(acmReportVisit, ReportVisitDTO.class)).willReturn(acmReportVisitDTO);

		// WHEN
		List<ReportVisitDTO> result = acmReportVisitService.find(acmReportVisitDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success save.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessSave() {

		// GIVEN
		ReportVisitDTO acmReportVisitDTO = initAcmReportVisitDTO();
		ReportVisit acmReportVisit = initAcmReportVisit();
		given(acmReportVisitRepository.save(any(ReportVisit.class))).willReturn(acmReportVisit);
		given(mapper.map(acmReportVisit, ReportVisitDTO.class)).willReturn(acmReportVisitDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		ReportVisitDTO acmReportVisitDTOReponse = acmReportVisitService.save(acmReportVisitDTO);

		// THEN
		assertThat(acmReportVisitDTOReponse).isNotNull();
	}

	/**
	 * Should success update.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		ReportVisitDTO acmReportVisitDTO = initAcmReportVisitDTO();
		acmReportVisitDTO.setIdReportVisit(new Long(1));
		ReportVisit acmReportVisit = mapper.map(acmReportVisitDTO, ReportVisit.class);
		acmReportVisit.setAcmVersion(0);
		given(acmReportVisitRepository.findById(any(Long.class)))
				.willReturn(Optional.of(acmReportVisit));
		given(acmReportVisitRepository.save(any(ReportVisit.class))).willReturn(acmReportVisit);
		given(mapper.map(acmReportVisit, ReportVisitDTO.class)).willReturn(acmReportVisitDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		ReportVisitDTO acmReportVisitDTOReponse =
				acmReportVisitService.save(acmReportVisit.getIdReportVisit(), acmReportVisitDTO);

		// THEN
		assertThat(acmReportVisitDTOReponse).isNotNull();
	}

	/**
	 * Should success find acm report visit by ID.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindAcmReportVisitByID() throws ResourcesNotFoundException {

		// GIVEN
		given(acmReportVisitRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initAcmReportVisit()));

		// WHEN
		ReportVisitDTO acmReportVisitDTO = acmReportVisitService.find(new Long(1));

		// THEN
		assertThat(acmReportVisitDTO).isNotNull();
	}

	/**
	 * Should not success find acm report visit by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldNotSuccessFindAcmReportVisitByID() throws ResourcesNotFoundException {

		// GIVEN
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			acmReportVisitService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("ReportVisit with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success delete.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessDelete() {

		// GIVEN
		ReportVisitDTO acmReportVisitDTO = initAcmReportVisitDTO();
		acmReportVisitDTO.setIdReportVisit(new Long(1));

		// WHEN
		acmReportVisitService.delete(acmReportVisitDTO);

		// THEN
		verify(acmReportVisitRepository, times(1)).deleteById(any(Long.class));
	}

	/**
	 * Inits the acm report visit.
	 * 
	 * @author HaythemBenizid
	 * @return the acm report visit
	 */
	private ReportVisit initAcmReportVisit() {

		ReportVisit acmReportVisit = new ReportVisit();
		acmReportVisit.setAcmVersion(0);
		return acmReportVisit;
	}

	/**
	 * Inits the acm report visit DTO.
	 * 
	 * @author YesserSomai
	 * @return the acm report visit DTO
	 */
	private ReportVisitDTO initAcmReportVisitDTO() {

		ReportVisitDTO acmReportVisitDTO = new ReportVisitDTO();
		acmReportVisitDTO.setIdReportVisit(new Long(1));
		acmReportVisitDTO.setDescription("Test Desc 1");
		acmReportVisitDTO.setIdLoan(new Long(5));
		return acmReportVisitDTO;
	}
}
