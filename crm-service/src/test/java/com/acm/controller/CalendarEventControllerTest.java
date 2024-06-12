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
import com.acm.service.CalendarEventService;
import com.acm.utils.dtos.CalendarEventDTO;

/**
 * The class {@link CalendarEventControllerTest}.
 *
 * @author MoezMhiri
 * @since 1.0.8
 */
public class CalendarEventControllerTest {

	/** The calendar event controller. */
	@InjectMocks
	private CalendarEventController calendarEventController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The calendar event service. */
	@Mock
	private CalendarEventService calendarEventService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(calendarEventController).build();
	}

	/**
	 * Creates the calendarEvent DTO.
	 * 
	 * @author MoezMhiri
	 * @return the calendarEvent DTO
	 */
	private CalendarEventDTO createCalendarEventDTO() {

		CalendarEventDTO calendarEventDTO = new CalendarEventDTO();
		calendarEventDTO.setId(new Long(1));
		return calendarEventDTO;
	}

	/**
	 * Should success find calendarEvent by id.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCalendarEventById() throws Exception {

		// GIVEN
		CalendarEventDTO calendarEventDTO = createCalendarEventDTO();
		given(calendarEventService.find(any(Long.class))).willReturn(calendarEventDTO);

		// WHEN
		this.mockMvc.perform(get("/calendar-events/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(calendarEventService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(calendarEventService);
	}

	/**
	 * Should return list calendarEvent DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCalendarEventDTO() throws Exception {

		// GIVEN
		CalendarEventDTO calendarEventDTO = new CalendarEventDTO();
		given(calendarEventService.find(any(CalendarEventDTO.class)))
				.willReturn(Collections.singletonList(calendarEventDTO));

		// WHEN
		this.mockMvc
				.perform(post("/calendar-events/").content(CommonFunctions.toJson(calendarEventDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(calendarEventService, times(1)).find(any(CalendarEventDTO.class));
		verifyNoMoreInteractions(calendarEventService);
	}

	/**
	 * Should success save calendarEvent.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveCalendarEvent() throws Exception {

		// WHEN
		given(calendarEventService.save(any(CalendarEventDTO.class)))
				.willReturn(new CalendarEventDTO());

		// WHEN
		this.mockMvc
				.perform(post("/calendar-events/create")
						.content(CommonFunctions.toJson(new CalendarEventDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(calendarEventService, times(1)).save(any(CalendarEventDTO.class));
		verifyNoMoreInteractions(calendarEventService);
	}

	/**
	 * Should success update.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		CalendarEventDTO calendarEventDTO = createCalendarEventDTO();
		given(calendarEventService.save(calendarEventDTO.getId(), calendarEventDTO))
				.willReturn(calendarEventDTO);

		// WHEN
		this.mockMvc
				.perform(put("/calendar-events/update")
						.content(CommonFunctions.toJson(calendarEventDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(calendarEventService, times(1)).save(any(Long.class), any(CalendarEventDTO.class));
		verifyNoMoreInteractions(calendarEventService);
	}

	/**
	 * Should success delete calendarEvent.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteCalendarEvent() throws Exception {

		// GIVEN
		doNothing().when(calendarEventService).delete(any(CalendarEventDTO.class));

		// WHEN
		this.mockMvc.perform(delete("/calendar-events/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(calendarEventService).delete(any(CalendarEventDTO.class));
		verifyNoMoreInteractions(calendarEventService);
	}

}
