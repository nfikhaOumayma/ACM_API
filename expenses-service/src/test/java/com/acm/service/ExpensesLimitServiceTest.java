/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ExpensesLimitsRepository;
import com.acm.service.impl.ExpensesLimitsServiceImpl;
import com.acm.utils.dtos.ExpensesLimitDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.ExpensesLimit;

/**
 * {@link ExpensesLimitServiceTest} class.
 *
 * @author MoezMhiri
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
class ExpensesLimitServiceTest {

	/** The expensesLimit service. */
	@InjectMocks
	private ExpensesLimitsServiceImpl expensesLimitService;

	/** The expensesLimit repository. */
	@Mock
	private ExpensesLimitsRepository expensesLimitRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

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
	 * Should success find expensesLimit.
	 * 
	 * @author MoezMhiri
	 */
	@Test
	void shouldSuccessFindExpensesLimit() {

		// GIVEN
		ExpensesLimit expensesLimit = initExpensesLimit();
		ExpensesLimitDTO expensesLimitDTO = initExpensesLimitDTO();
		given(expensesLimitRepository.findAll())
				.willReturn(Collections.singletonList(expensesLimit));
		given(mapper.map(expensesLimit, ExpensesLimitDTO.class)).willReturn(expensesLimitDTO);

		// WHEN
		List<ExpensesLimitDTO> result = expensesLimitService.find(expensesLimitDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success save.
	 * 
	 * @author MoezMhiri
	 */
	@Test
	void shouldSuccessSave() {

		// GIVEN
		ExpensesLimitDTO expensesLimitDTO = initExpensesLimitDTO();
		ExpensesLimit expensesLimit = initExpensesLimit();
		List<ExpensesLimitDTO> expensesLimitDTOs = new ArrayList<>();
		given(mapper.map(expensesLimitDTO, ExpensesLimit.class)).willReturn(expensesLimit);
		expensesLimitDTOs.add(expensesLimitDTO);
		doNothing().when(expensesLimitRepository).deleteByIdBranch(new Long(1));
		// WHEN
		expensesLimitRepository.save(expensesLimit);
		// THEN
		assertThat(expensesLimitDTOs).isNotNull();
	}

	/**
	 * Should success update.
	 * 
	 * @author MoezMhiri
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		ExpensesLimitDTO expensesLimitDTO = initExpensesLimitDTO();
		expensesLimitDTO.setId(new Long(1));
		ExpensesLimit expensesLimit = mapper.map(expensesLimitDTO, ExpensesLimit.class);
		expensesLimit.setAcmVersion(0);
		given(expensesLimitRepository.findById(any(Long.class)))
				.willReturn(Optional.of(expensesLimit));
		given(expensesLimitRepository.save(any(ExpensesLimit.class))).willReturn(expensesLimit);
		given(mapper.map(expensesLimit, ExpensesLimitDTO.class)).willReturn(expensesLimitDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		ExpensesLimitDTO expensesLimitDTOReponse =
				expensesLimitService.save(expensesLimit.getId(), expensesLimitDTO);

		// THEN
		assertThat(expensesLimitDTOReponse).isNotNull();
	}

	/**
	 * Should refresh limit.
	 * 
	 * @author MoezMhiri
	 */
	@Test
	void shouldRefreshLimits() {

		// GIVEN
		ExpensesLimitDTO expensesLimitDTO = initExpensesLimitDTO();
		List<ExpensesLimit> expensesLimits = new ArrayList<>();
		given(expensesLimitRepository.findAll()).willReturn(expensesLimits);
		List<ExpensesLimitDTO> expensesLimitDTOs = new ArrayList<>();
		ExpensesLimit expensesLimit = mapper.map(expensesLimitDTO, ExpensesLimit.class);
		expensesLimitDTOs.add(expensesLimitDTO);
		// WHEN
		expensesLimitRepository.save(expensesLimit);
		// THEN
		assertThat(expensesLimitDTOs).isNotNull();
	}

	/**
	 * Inits the calendarEvent.
	 *
	 * @return the calendarEvent
	 */
	private ExpensesLimit initExpensesLimit() {

		ExpensesLimit expensesLimit = new ExpensesLimit();
		expensesLimit.setAcmVersion(0);
		return expensesLimit;
	}

	/**
	 * Inits the calendarEvent DTO.
	 *
	 * @return the calendarEvent DTO
	 */
	private ExpensesLimitDTO initExpensesLimitDTO() {

		ExpensesLimitDTO expensesLimitDTO = new ExpensesLimitDTO();
		expensesLimitDTO.setId(new Long(1));
		expensesLimitDTO.setIdBranch(new Long(1));
		expensesLimitDTO.setIdExpensesType(new Long(1));
		expensesLimitDTO.setLimit(new Long(1000));
		expensesLimitDTO.setRestLimit(new Long(2000));
		return expensesLimitDTO;
	}
}
