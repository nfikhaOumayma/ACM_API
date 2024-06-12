/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmMezaCardRepository;
import com.acm.service.impl.MezaCardServiceImpl;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.AcmMezaCardPaginationDTO;
import com.acm.utils.models.AcmMezaCard;
import com.acm.utils.models.Customer;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link MezaCardServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class MezaCardServiceTest {

	/** The meza card service. */
	@InjectMocks
	private MezaCardServiceImpl mezaCardService;

	/** The acm meza card repository. */
	@Mock
	private AcmMezaCardRepository acmMezaCardRepository;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The credit client. */
	@Mock
	private CreditClient creditClient;

	/** The parametrage client. */
	@Mock
	private ParametrageClient parametrageClient;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The entity manager. */
	@Spy
	private EntityManager entityManager;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
		entityManager = Mockito.mock(EntityManager.class);

	}

	/**
	 * Inits the acm meza card DTO.
	 * 
	 * @author ManelLamloum
	 * @return the acm meza card DTO
	 */
	AcmMezaCardDTO initAcmMezaCardDTO() {

		AcmMezaCardDTO acmMezaCardDTO = new AcmMezaCardDTO();
		acmMezaCardDTO.setMerchantID(new BigDecimal(1));
		acmMezaCardDTO.setCardNumber("123456");
		acmMezaCardDTO.setIdMezaCard(new Long(1));
		acmMezaCardDTO.setBranchID(new Long(1));
		acmMezaCardDTO.setStatus("status-test");
		return acmMezaCardDTO;
	}

	/**
	 * Inits the acm meza card.
	 * 
	 * @author ManelLamloum
	 * @return the acm meza card
	 */
	AcmMezaCard initAcmMezaCard() {

		AcmMezaCard acmMezaCard = new AcmMezaCard();
		acmMezaCard.setCardNumber("123456");
		acmMezaCard.setIdMezaCard(new Long(1));
		acmMezaCard.setStatus("new");
		acmMezaCard.setBranchID(new Long(1));
		acmMezaCard.setMerchantID(new BigDecimal(1));
		return acmMezaCard;
	}

	/**
	 * Inits the user DTO.
	 * 
	 * @author ManelLamloum
	 * @return the user DTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin(CommonConstants.DEFAULT_USER);
		userDTO.setPrenom("benTest");
		userDTO.setFullName("Test Ben Test Test");
		Set<GroupeDTO> groupesDTO = new HashSet<GroupeDTO>();
		GroupeDTO groupeDTO = new GroupeDTO();
		groupeDTO.setId(new Long(1));
		groupeDTO.setCode("TEST");
		groupesDTO.add(groupeDTO);
		userDTO.setGroupes(groupesDTO);
		return userDTO;
	}

	/**
	 * Should success find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindById() throws ResourcesNotFoundException {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		given(acmMezaCardRepository.findById(any(Long.class))).willReturn(Optional.of(acmMezaCard));
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);

		// WHEN
		AcmMezaCardDTO result = mezaCardService.find(new Long(1));
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success return list of acm meza card pagination.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessReturnListOfAcmMezaCardPagination() {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		Page<AcmMezaCard> pagedResult = new Page<AcmMezaCard>() {

			@Override
			public int getNumber() {

				return 0;
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
			public List<AcmMezaCard> getContent() {

				return null;
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
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Iterator<AcmMezaCard> iterator() {

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

			@Override
			public <U> Page<U> map(Function<? super AcmMezaCard, ? extends U> converter) {

				return null;
			}
		};

		AcmMezaCardPaginationDTO acmMezaCardPaginationDTO = new AcmMezaCardPaginationDTO();
		acmMezaCardPaginationDTO.setParams(acmMezaCardDTO);
		given(acmMezaCardRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);

		// WHEN
		AcmMezaCardPaginationDTO result = mezaCardService.find(acmMezaCardPaginationDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success create list meza card.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessCreateListMezaCard() throws ResourcesNotFoundException {

		// GIVEN
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmMezaCardRepository.findById(any(Long.class))).willReturn(Optional.of(acmMezaCard));
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);
		given(acmMezaCardRepository.save(acmMezaCard)).willReturn(acmMezaCard);

		// WHEN
		List<AcmMezaCardDTO> result =
				mezaCardService.save(Collections.singletonList(acmMezaCardDTO));

		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success find by branch ID and status.
	 * 
	 * @author ManelLamloum
	 */

	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessFindByBranchIDAndStatus() {

		// GIVEN
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmMezaCardRepository.findFirstByBranchIDAndStatusAndCardNumberNotInOrderByCardNumber(
				any(Long.class), any(String.class), (List<String>) any(Object.class)))
						.willReturn(acmMezaCard);
		given(parametrageClient.findAndAddIfNotExist(any(String.class), any(String.class)))
				.willReturn(Boolean.FALSE);
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);

		// WHEN
		AcmMezaCardDTO result = mezaCardService.findByBranchIDAndStatus(acmMezaCardDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update acm meza card.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessSaveAcmMezaCard() throws ResourcesNotFoundException {

		// GIVEN
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmMezaCardRepository.findById(any(Long.class))).willReturn(Optional.of(acmMezaCard));
		given(acmMezaCardRepository.save(any(AcmMezaCard.class))).willReturn((acmMezaCard));
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);

		// WHEN
		AcmMezaCardDTO result =
				mezaCardService.save(acmMezaCardDTO.getIdMezaCard(), acmMezaCardDTO);

		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update.
	 *
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmMezaCardRepository.save(any(AcmMezaCard.class))).willReturn(acmMezaCard);

		given(acmMezaCardRepository.findById(any(Long.class))).willReturn(Optional.of(acmMezaCard));
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);
		doReturn(acmMezaCard).when(mapper).map(acmMezaCardDTO, AcmMezaCard.class);
		given(acmMezaCardRepository.findAll(any(BooleanBuilder.class)))
				.willReturn(Collections.singletonList(acmMezaCard));

		doNothing().when(acmMezaCardRepository).deleteById(any(Long.class));
		// WHEN
		AcmMezaCardDTO result = mezaCardService.update(acmMezaCardDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success find.
	 */
	@Test
	void shouldSuccessFind() {

		// GIVEN
		AcmMezaCard acmMezaCard = initAcmMezaCard();
		AcmMezaCardDTO acmMezaCardDTO = initAcmMezaCardDTO();
		given(acmMezaCardRepository.findAll()).willReturn(Collections.singletonList(acmMezaCard));
		doReturn(acmMezaCardDTO).when(mapper).map(acmMezaCard, AcmMezaCardDTO.class);

		// WHEN
		List<AcmMezaCardDTO> result = mezaCardService.find(acmMezaCardDTO);

		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success find meza customers.
	 */
	@Test
	void shouldSuccessFindMezaCustomers() {

		// GIVEN
		BigInteger i = new BigInteger("0");
		doReturn(Collections.singletonList(i)).when(Mockito.mock(Query.class)).getResultList();

		// WHEN
		List<Customer> result = mezaCardService.findMezaCustomers();

		// THEN
		assertThat(result).isNotNull();
	}
}
