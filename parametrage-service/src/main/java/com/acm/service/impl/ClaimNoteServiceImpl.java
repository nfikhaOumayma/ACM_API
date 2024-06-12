/*
 * 
 */
package com.acm.service.impl;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.client.CreditClient;
import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ClaimNoteRepository;
import com.acm.service.ClaimNoteService;
import com.acm.utils.dtos.ClaimNoteDTO;
import com.acm.utils.models.AcmCollateral;
import com.acm.utils.models.ClaimNote;
import com.acm.utils.models.QClaimNote;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

// TODO: Auto-generated Javadoc
/**
 * The Class ClaimNoteServiceImpl.
 */
@Service
public class ClaimNoteServiceImpl implements ClaimNoteService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ClaimNoteServiceImpl.class);

	/** The claim note repository. */
	@Autowired
	private ClaimNoteRepository claimNoteRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The ib api url. */
	@Value("${ib.environment.apiUrl}")
	private String ibApiUrl;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	@Autowired
	private ReportingClient reportingClient;

	/**
	 * Find.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Override
	public List<ClaimNoteDTO> find(ClaimNoteDTO claimNoteDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(claimNoteDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// if id claim is null
		if (ACMValidationUtils.isNullOrEmpty(claimNoteDTO.getClaimId())) {
			return new ArrayList<>();
		}

		QClaimNote qClaimNote = QClaimNote.claimNote;
		BooleanBuilder predicate = new BooleanBuilder();
		predicate.and(qClaimNote.enabled.eq(Boolean.TRUE));

		// find by claim id
		if (!ACMValidationUtils.isNullOrEmpty(claimNoteDTO.getClaimId())) {
			predicate.and(qClaimNote.claimId.eq(claimNoteDTO.getClaimId()));
		}

		Iterable<ClaimNote> iterable = claimNoteRepository.findAll(predicate);
		List<ClaimNote> claimNotes = new ArrayList<>();
		iterable.forEach(claimNotes::add);
		logger.info("{} : claim notes was founded", claimNotes.size());

		List<ClaimNoteDTO> claimNotesDTOs = new ArrayList<>();
		claimNotes.forEach(note -> claimNotesDTOs.add(mapper.map(note, ClaimNoteDTO.class)));
		return claimNotesDTOs;

	};

	/**
	 * Save.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the claim note DTO
	 */
	@Override
	public ClaimNoteDTO save(ClaimNoteDTO claimNoteDTO, String categorie) {

		Preconditions.checkNotNull(claimNoteDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ClaimNote claimNote = mapper.map(claimNoteDTO, ClaimNote.class);
		CommonFunctions.mapperToSave(claimNote, userClient, logger);
		ClaimNote result = claimNoteRepository.save(claimNote);

		if (claimNoteDTO.getVisibility().equals("public") && categorie.equals("CUSTOMER")) {
			try {
				String apiUrl = ibApiUrl + "parametrage-service/claim-note/save-note-ib";
				URI uri = new URI(apiUrl);
				HttpHeaders headers = new HttpHeaders();
				headers.set("Authorization", "Bearer " + creditClient.loginApiIb());
				headers.setContentType(MediaType.APPLICATION_JSON);
				headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
				RestTemplate restTemplate = new RestTemplate();

				HttpEntity<ClaimNoteDTO> request = new HttpEntity<>(claimNoteDTO, headers);
				ResponseEntity<ClaimNoteDTO> responseAPI =
						restTemplate.exchange(uri, HttpMethod.POST, request, ClaimNoteDTO.class);

			}
			catch (Exception e) {
				logger.error(e.getMessage());
			}
		}
		// else if (claimNoteDTO.getVisibility().equals("public") && categorie.equals("PROSPECT")) {
		// MailDTO mailDto = new
		// MailDTO("devops@talys.digital","bechahazem390@gmail.com","TEST","content.");
		// reportingClient.sendMail(mailDto);
		// }

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmCollateral.class.getSimpleName());
		return mapper.map(result, ClaimNoteDTO.class);
	};

	/**
	 * Save note from ib.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the claim note DTO
	 */
	@Override
	public ClaimNoteDTO saveNoteFromIb(ClaimNoteDTO claimNoteDTO) {

		Preconditions.checkNotNull(claimNoteDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ClaimNote claimNote = mapper.map(claimNoteDTO, ClaimNote.class);
		CommonFunctions.mapperToSave(claimNote, userClient, logger);
		claimNote.setInsertBy(claimNoteDTO.getInsertBy());
		ClaimNote result = claimNoteRepository.save(claimNote);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ClaimNoteDTO.class.getSimpleName());
		return mapper.map(result, ClaimNoteDTO.class);
	};

}
