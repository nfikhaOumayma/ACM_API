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

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.UserService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.UserPaginationDTO;
import com.acm.utils.enums.UserCategory;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The Class UserControllerTest.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
class UserControllerTest {

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The user controller. */
	@InjectMocks
	private UserController userController;

	/** The user service. */
	@Mock
	private UserService userService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The principal. */
	@Mock
	private Principal principal;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
		this.mockMvc = MockMvcBuilders.standaloneSetup(userController).build();
	}

	/**
	 * Creates the user DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the user DTO
	 */
	private UserDTO createUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setLogin("admin");
		return userDTO;
	}

	/**
	 * To json.
	 *
	 * @author HaythemBenizid
	 * @param object the object
	 * @return the byte[]
	 * @throws Exception the exception
	 */
	private byte[] toJson(Object object) throws Exception {

		ObjectMapper map = new ObjectMapper();
		return map.writeValueAsBytes(object);
	}

	/**
	 * Should succefly find connected user.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void ShouldSucceflyFindConnectedUser() throws Exception {

		// GIVEN
		UserDTO userDTO = createUserDTO();
		given(userService.find(principal.getName())).willReturn(userDTO);

		// WHEN
		this.mockMvc.perform(
				get("/users/connected").principal(principal).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(userService, times(1)).find(principal.getName());
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should succefly find by login.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void ShouldSucceflyFindByLogin() throws Exception {

		// GIVEN
		UserDTO userDTO = createUserDTO();
		given(userService.find("admin")).willReturn(userDTO);

		// WHEN
		this.mockMvc.perform(get("/users/admin").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).find("admin");
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should succeflyfind user principal.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void ShouldSucceflyFindUserPrincipal() throws Exception {

		// GIVEN
		Principal principal = new Principal() {

			@Override
			public String getName() {

				return "test";
			}
		};

		// WHEN
		this.mockMvc.perform(
				get("/users/principal").principal(principal).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
	}

	/**
	 * Should return list user DTO.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListUserDTO() throws Exception {

		// GIVEN
		UserDTO userDTO = new UserDTO();
		given(userService.find(any(UserDTO.class))).willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc
				.perform(post("/users/").content(toJson(userDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userService, times(1)).find(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success save user.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveUser() throws Exception {

		// GIVEN
		given(userService.save(any(UserDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/create").content(toJson(createUserDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).save(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success save user for IB.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveUserForIB() throws Exception {

		// GIVEN
		given(userService.saveForIB(any(UserDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/create-for-ib").content(toJson(createUserDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).saveForIB(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success save user for batch.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveUserForBatch() throws Exception {

		// GIVEN
		given(userService.saveForBatch(any(UserDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/create-for-batch").content(toJson(createUserDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).saveForBatch(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success find users.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUsers() throws Exception {

		// GIVEN
		UserDTO userDTO = new UserDTO();
		given(userService.findUsers(Boolean.FALSE)).willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc
				.perform(get("/users/findUsers").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userService, times(1)).findUsers(Boolean.FALSE);
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success find users full list.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUsersFullList() throws Exception {

		// GIVEN
		UserDTO userDTO = new UserDTO();
		given(userService.findUsers(Boolean.TRUE)).willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc
				.perform(get("/users/find-full-list").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userService, times(1)).findUsers(Boolean.TRUE);
		verifyNoMoreInteractions(userService);
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
		acmStatutsDTOs.add(new AcmStatutsDTO(1, UserCategory.OPERATION.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(2, UserCategory.MANAGMENT.name()));

		// WHEN
		this.mockMvc
				.perform(get("/users/user-category").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		assertTrue(acmStatutsDTOs.size() == 2);
		// Test class property, and its value
		assertThat(acmStatutsDTOs,
				containsInAnyOrder(hasProperty("value", is(UserCategory.OPERATION.name())),
						hasProperty("value", is(UserCategory.MANAGMENT.name()))));
	}

	/**
	 * Should success save enable.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveEnable() throws Exception {

		// GIVEN
		given(userService.saveEnable(any(UserDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/enable").content(toJson(createUserDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).saveEnable(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
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
		UserDTO userDTO = createUserDTO();
		given(userService.save(userDTO.getLogin(), userDTO)).willReturn(userDTO);

		// WHEN
		this.mockMvc
				.perform(post("/users/update").content(toJson(userDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).save(any(String.class), any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success find users by branch id.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUsersByBranchId() throws Exception {

		// GIVEN
		UserDTO userDTO = new UserDTO();
		given(userService.findUsersByBranchId()).willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc
				.perform(get("/users/findUsersBranch").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userService, times(1)).findUsersByBranchId();
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should return find portfolio.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindPortfolio() throws Exception {

		// GIVEN
		UserDTO userDTO = new UserDTO();
		given(userService.findPortfolio(any(UserDTO.class)))
				.willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc
				.perform(post("/users/find-portfolio").content(toJson(userDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userService, times(1)).findPortfolio(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should return find by groupe.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindByGroupe() throws Exception {

		// GIVEN
		UserDTO userDTO = new UserDTO();
		given(userService.findByGroupe(any(UserDTO.class)))
				.willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc
				.perform(post("/users/find-by-groupe").content(toJson(userDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userService, times(1)).findByGroupe(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success find by groupe code and branch ID.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindByGroupeCodeAndBranchID() throws Exception {

		// GIVEN
		UserDTO userDTO = createUserDTO();
		given(userService.findByGroupeCodeAndBranchID(any(String.class), any(Integer.class)))
				.willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc.perform(
				get("/users/find-by-groupe-branch/ADMIN/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).findByGroupeCodeAndBranchID(any(String.class),
				any(Integer.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success reset PWD.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessResetPWD() throws Exception {

		// GIVEN
		UserDTO userDTO = createUserDTO();
		given(userService.resetPWD(userDTO.getLogin())).willReturn(userDTO);

		// WHEN
		this.mockMvc
				.perform(post("/users/forget-pwd").content(toJson(userDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).resetPWD(any(String.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success update pwd.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdatePwd() throws Exception {

		// GIVEN
		given(userService.updatePWD(any(UserDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/update-pwd").content(toJson(createUserDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).updatePWD(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success find users pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUsersPagination() throws Exception {

		// GIVEN
		given(userService.find(any(UserPaginationDTO.class))).willReturn(new UserPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/find-pagination").content(toJson(new UserPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).find(any(UserPaginationDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success save default lang.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveDefaultLang() throws Exception {

		// GIVEN
		given(userService.saveDefaultLang(any(UserDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/users/set-default-lang").content(toJson(createUserDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).saveDefaultLang(any(UserDTO.class));
		verifyNoMoreInteractions(userService);
	}

	/**
	 * Should success find by groupe code and branch ID and access branches.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindByGroupeCodeAndBranchIDAndAccessBranches() throws Exception {

		// GIVEN
		UserDTO userDTO = createUserDTO();
		given(userService.findByGroupeCodeAndBranchIDAndAccessBranches(any(String.class),
				any(Integer.class))).willReturn(Collections.singletonList(userDTO));

		// WHEN
		this.mockMvc.perform(get("/users/find-by-groupe-branch-access-branches/ADMIN/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(userService, times(1)).findByGroupeCodeAndBranchIDAndAccessBranches(
				any(String.class), any(Integer.class));
		verifyNoMoreInteractions(userService);
	}
}
