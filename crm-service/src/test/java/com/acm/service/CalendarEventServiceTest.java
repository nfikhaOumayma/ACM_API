/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

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
import com.acm.repository.CalendarEventRepository;
import com.acm.service.impl.CalendarEventServiceImpl;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.CalendarEvent;

/**
 * {@link CalendarEventServiceTest} class.
 *
 * @author MoezMhiri
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
class CalendarEventServiceTest {

	/** The calendarEvent service. */
	@InjectMocks
	private CalendarEventServiceImpl calendarEventService;

	/** The calendarEvent repository. */
	@Mock
	private CalendarEventRepository calendarEventRepository;

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
	 * Should success save.
	 * 
	 * @author MoezMhiri
	 */
	@Test
	void shouldSuccessSave() {

		// GIVEN
		CalendarEventDTO calendarEventDTO = initCalendarEventDTO();
		CalendarEvent calendarEvent = initCalendarEvent();
		given(calendarEventRepository.save(any(CalendarEvent.class))).willReturn(calendarEvent);
		given(mapper.map(calendarEvent, CalendarEventDTO.class)).willReturn(calendarEventDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		CalendarEventDTO calendarEventDTOReponse = calendarEventService.save(calendarEventDTO);

		// THEN
		assertThat(calendarEventDTOReponse).isNotNull();
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
		CalendarEventDTO calendarEventDTO = initCalendarEventDTO();
		calendarEventDTO.setId(new Long(1));
		CalendarEvent calendarEvent = mapper.map(calendarEventDTO, CalendarEvent.class);
		calendarEvent.setAcmVersion(0);
		given(calendarEventRepository.findById(any(Long.class)))
				.willReturn(Optional.of(calendarEvent));
		given(calendarEventRepository.save(any(CalendarEvent.class))).willReturn(calendarEvent);
		given(mapper.map(calendarEvent, CalendarEventDTO.class)).willReturn(calendarEventDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		CalendarEventDTO calendarEventDTOReponse =
				calendarEventService.save(calendarEvent.getId(), calendarEventDTO);

		// THEN
		assertThat(calendarEventDTOReponse).isNotNull();
	}

	/**
	 * Should success find calendar event by ID.
	 *
	 * @author MoezMhiri
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindCalendarEventByID() throws ResourcesNotFoundException {

		// GIVEN
		given(calendarEventRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initCalendarEvent()));

		// WHEN
		CalendarEventDTO calendarEventDTO = calendarEventService.find(new Long(1));

		// THEN
		assertThat(calendarEventDTO).isNotNull();
	}

	/**
	 * Should success find calendarEvent.
	 * 
	 * @author MoezMhiri
	 */
	@Test
	void shouldSuccessFindCalendarEvent() {

		// GIVEN
		CalendarEvent calendarEvent = initCalendarEvent();
		CalendarEventDTO calendarEventDTO = initCalendarEventDTO();
		given(mapper.map(calendarEvent, CalendarEventDTO.class)).willReturn(calendarEventDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		List<CalendarEventDTO> result = calendarEventService.find(calendarEventDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success delete.
	 * 
	 * @author MoezMhiri
	 */
	@Test
	void shouldSuccessDelete() {

		// GIVEN
		CalendarEventDTO calendarEventDTO = initCalendarEventDTO();
		calendarEventDTO.setId(new Long(1));

		// GIVEN

		// WHEN
		calendarEventService.delete(calendarEventDTO);

		// THEN
		verify(calendarEventRepository, times(1)).deleteById(any(Long.class));
	}

	/**
	 * Inits the acm report visit.
	 *
	 * @return the acm report visit
	 */
	private CalendarEvent initCalendarEvent() {

		CalendarEvent calendarEvent = new CalendarEvent();
		calendarEvent.setAcmVersion(0);
		return calendarEvent;
	}

	/**
	 * Inits the calendar event DTO.
	 *
	 * @return the calendar event DTO
	 */
	private CalendarEventDTO initCalendarEventDTO() {

		CalendarEventDTO calendarEventDTO = new CalendarEventDTO();
		calendarEventDTO.setId(new Long(1));
		calendarEventDTO.setLibelleEvent("Test Title 1");
		calendarEventDTO.setDescription("Test Desc 1");
		calendarEventDTO.setUsername("Test");
		return calendarEventDTO;
	}
}
