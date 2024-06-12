/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.type.GroupeUsersFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.GroupeRepository;
import com.acm.service.impl.GroupeServiceImpl;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.GroupePaginationDTO;
import com.acm.utils.models.Groupe;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link GroupeServiceTest } class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class GroupeServiceTest {

	/** The groupe service. */
	@InjectMocks
	private GroupeServiceImpl groupeService;

	/** The groupe repository. */
	@Mock
	private GroupeRepository groupeRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The habilitation service. */
	@Mock
	private HabilitationService habilitationService;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the groupe.
	 *
	 * @return the groupe
	 */
	private Groupe initGroupe() {

		Groupe groupe = new Groupe();
		groupe.setId(new Long(1));
		groupe.setCode("test");
		return groupe;
	}

	/**
	 * Inits the groupe DTO.
	 *
	 * @return the groupe DTO
	 */
	private GroupeDTO initGroupeDTO() {

		GroupeDTO groupeDTO = new GroupeDTO();
		groupeDTO.setId(new Long(1));
		groupeDTO.setCode("TEST");
		return groupeDTO;
	}

	/**
	 * Inits the user DTO.
	 *
	 * @return the user DTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin(CommonConstants.DEFAULT_USER);
		userDTO.setPrenom("benTest");
		userDTO.setFullName("Test Ben Test Test");
		return userDTO;
	}

	/**
	 * Should return list of group DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListOfGroupDTO() {

		// GIVEN
		GroupeDTO groupeDTO = new GroupeDTO();
		Groupe groupe = new Groupe();
		given(groupeRepository.findAll()).willReturn(Collections.singletonList(groupe));
		given(mapper.map(groupe, GroupeDTO.class)).willReturn(groupeDTO);
		// WHEN
		List<GroupeDTO> GroupeDTOs = groupeService.find(groupeDTO);
		// THEN
		Assertions.assertThat(GroupeDTOs).isNotNull();
	}

	/**
	 * Should success update groupe DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateGroupeDTO() throws ResourcesNotFoundException {

		// GIVEN
		GroupeDTO groupeDTO = initGroupeDTO();
		Groupe groupe = initGroupe();
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(Groupe.class), CommonAOPConstants.UPDATE,
				new Long(1), CommonFunctions.convertObjectToJSONString(groupe));
		given(groupeRepository.findById(new Long(1))).willReturn(Optional.of(groupe));
		given(groupeRepository.save(groupe)).willReturn(groupe);
		given(mapper.map(groupe, GroupeDTO.class)).willReturn(groupeDTO);
		given(settingHistoriqueService.save(settingHistoriqueDTO)).willReturn(settingHistoriqueDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		GroupeDTO result = groupeService.save(new Long(1), groupeDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success update enabled.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GroupeUsersFoundException the groupe users found exception
	 */
	@Test
	void shouldSuccessUpdateEnabled() throws ResourcesNotFoundException, GroupeUsersFoundException {

		// GIVEN
		GroupeDTO groupeDTO = initGroupeDTO();
		groupeDTO.setEnabled(Boolean.TRUE);
		Groupe groupe = initGroupe();
		given(groupeRepository.findById(groupe.getId())).willReturn(Optional.of(groupe));
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(Groupe.class), CommonAOPConstants.UPDATE,
				new Long(1), CommonFunctions.convertObjectToJSONString(groupe));
		given(groupeRepository.findById(new Long(1))).willReturn(Optional.of(groupe));
		given(mapper.map(groupe, GroupeDTO.class)).willReturn(groupeDTO);
		given(groupeRepository.save(groupe)).willReturn(groupe);
		given(settingHistoriqueService.save(settingHistoriqueDTO)).willReturn(settingHistoriqueDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// THEN
		GroupeDTO result = groupeService.updateEnabled(groupeDTO);
		// WHEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should success create groupe DTO.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessCreateGroupeDTO() throws ResourcesNotFoundException {

		// GIVEN
		GroupeDTO groupeDTO = initGroupeDTO();

		Groupe groupe = initGroupe();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		doReturn(groupeDTO).when(mapper).map(groupe, GroupeDTO.class);
		doReturn(groupe).when(mapper).map(groupeDTO, Groupe.class);
		doReturn(initUserDTO()).when(userClient).find();

		given(groupeRepository.save(groupe)).willReturn(groupe);
		given(habilitationService.findByGroupeID())
				.willReturn(Collections.singletonList(habilitationDTO));
		doNothing().when(habilitationService).saveAll(Collections.singletonList(habilitationDTO));
		// WHEN
		GroupeDTO result = groupeService.save(groupeDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should success find all pagination.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindAllPagination() {

		// GIVEN
		Groupe groupe = initGroupe();
		GroupeDTO groupeDTO = initGroupeDTO();
		given(groupeRepository.findAll()).willReturn(Collections.singletonList(groupe));
		doReturn(groupeDTO).when(mapper).map(groupe, GroupeDTO.class);

		// THEN
		List<GroupeDTO> result = groupeService.find();

		// WHEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find groupe pagination.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindGroupePagination() {

		// GIVEN
		Groupe groupe = initGroupe();
		GroupeDTO groupeDTO = initGroupeDTO();
		GroupePaginationDTO groupePaginationDTO = new GroupePaginationDTO();
		groupePaginationDTO.setParams(groupeDTO);
		Page<Groupe> pagedResult = new Page<Groupe>() {

			@Override
			public Iterator<Groupe> iterator() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasContent() {

				return false;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public List<Groupe> getContent() {

				return null;
			}

			@Override
			public <U> Page<U> map(Function<? super Groupe, ? extends U> converter) {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 0;
			}

			@Override
			public long getTotalElements() {

				return 0;
			}
		};
		given(groupeRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn((pagedResult));

		doReturn(groupeDTO).when(mapper).map(groupe, GroupeDTO.class);

		// WHEN
		GroupePaginationDTO result = groupeService.find(groupePaginationDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success build query.
	 * 
	 * @author ManelLamloum
	 */
	@Disabled // because buildQuery is private
	@Test
	void shouldSuccessBuildQuery() {

		// GIVEN
		// GroupeDTO groupeDTO = initGroupeDTO();
		// groupeDTO.setCode("TEST-CODE");
		// groupeDTO.setLibelle("TEST-LIBELLE");
		// groupeDTO.setDescription("TEST-DESCR");
		// groupeDTO.setEnabled(Boolean.TRUE);

		// WHEN
		// because buildQuery is private
		// groupeService.buildQuery(groupeDTO, qGroupe);
		// THEN
	}

	/**
	 * Should success find by code.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindByCode() throws ResourcesNotFoundException {

		// GIVEN
		UserDTO userDTO = initUserDTO();
		Groupe groupe = initGroupe();
		GroupeDTO groupeDTO = initGroupeDTO();
		given(groupeRepository.findByCode(any(String.class))).willReturn(groupe);
		doReturn(groupeDTO).when(mapper).map(groupe, GroupeDTO.class);
		given(userClient.findByGroupe(initUserDTO()))
				.willReturn(Collections.singletonList(userDTO));

		// WHEN
		GroupeDTO result = groupeService.findByCode("CODE");
		// THEN
		Assertions.assertThat(result).isNotNull();

	}
}
