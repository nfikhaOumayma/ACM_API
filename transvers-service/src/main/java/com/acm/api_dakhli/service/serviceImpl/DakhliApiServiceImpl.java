package com.acm.api_dakhli.service.serviceImpl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.acm.api_dakhli.service.DakhliApiService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ResponseIncomeDakhliApiDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;

// TODO: Auto-generated Javadoc
/**
 * The Class DakhliApiServiceImpl.
 */
@Service
public class DakhliApiServiceImpl implements DakhliApiService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(DakhliApiServiceImpl.class);

	/** The api url. */
	private String apiUrl = "https://dakhli-qa-api.apps.devocp4.elm.sa/dakhli/api/v1/";

	/** The app id. */
	private String appId = "019acf06";

	/** The app key. */
	private String appKey = "29b56dfcb8a760119740a850e94fd813";

	/** The platform key. */
	private String platformKey = "cd4dcce2-67a7-4d0b-afa9-48d95a8fab53";

	/** The organization number. */
	private String organizationNumber = "7000000000";

	/** The request reason. */
	private String requestReason = "xxxxxxxxxxxxxxxx";

	/** The rest template. */
	private final RestTemplate restTemplate = new RestTemplate();

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_dakhli.service.DakhliApiService#getIncome(java.lang.String, java.lang.Long)
	 */
	@Override
	public ResponseEntity<ResponseIncomeDakhliApiDTO> getIncome(String nationalId, Long loanId) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("DAKHLI_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_URL", apiUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_KEY", appKey);
		platformKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_PLATFORM_KEY", platformKey);
		organizationNumber = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_ORGANIZATION_NUMBER", organizationNumber);
		requestReason = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_REQUEST_REASON", requestReason);

		String url = apiUrl + "gosi/income/" + nationalId;

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("PLATFORM-KEY", platformKey);
		headers.add("ORGANIZATION-NUMBER", organizationNumber);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT DAKHLI Request For getEmploymentStatus API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<ResponseIncomeDakhliApiDTO> response = restTemplate.exchange(url,
					HttpMethod.GET, requestEntity, ResponseIncomeDakhliApiDTO.class);

			thirdPartyHistoDTO.setCategory("DAKHLI/Income");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT DAKHLI Response For getEmploymentStatus API = [{}] ### ",
					response);

			return ResponseEntity.ok(response.getBody());
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_dakhli.service.DakhliApiService#getEmploymentStatusRowad(java.lang.String)
	 */
	@Override
	public ResponseEntity<?> getIncomeRowad(String customerId) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("DAKHLI_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_URL", apiUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_KEY", appKey);
		platformKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_PLATFORM_KEY", platformKey);
		organizationNumber = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_ORGANIZATION_NUMBER", organizationNumber);
		requestReason = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_REQUEST_REASON", requestReason);

		String url = apiUrl + "rowad/gosi/income/" + customerId;

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("PLATFORM-KEY", platformKey);
		headers.add("ORGANIZATION-NUMBER", organizationNumber);
		headers.add("PACKAGE-NAME", platformKey);
		headers.add("REQUEST-REASON", requestReason);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT DAKHLI Request For getEmploymentStatusRowad API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<?> response =
					restTemplate.exchange(url, HttpMethod.GET, requestEntity, String.class);

			thirdPartyHistoDTO.setCategory("DAKHLI/IncomeRowad");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue((String) response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT DAKHLI Response For getEmploymentStatusRowad API = [{}] ### ",
					response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_dakhli.service.DakhliApiService#getGovernmentPayslip(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public ResponseEntity<?> getGovernmentPayslip(String id, String birthDate) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("DAKHLI_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_URL", apiUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_KEY", appKey);
		platformKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_PLATFORM_KEY", platformKey);
		organizationNumber = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_ORGANIZATION_NUMBER", organizationNumber);
		requestReason = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_REQUEST_REASON", requestReason);

		UriComponentsBuilder url = UriComponentsBuilder.fromUriString(apiUrl + "goverment/payslip")
				.queryParam("id", id).queryParam("birthDate", birthDate);

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("PLATFORM-KEY", platformKey);
		headers.add("ORGANIZATION-NUMBER", organizationNumber);
		headers.add("PACKAGE-NAME", platformKey);
		headers.add("REQUEST-REASON", requestReason);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT DAKHLI Request For getGovernmentPayslip API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<?> response = restTemplate.exchange(url.toUriString(), HttpMethod.GET,
					requestEntity, String.class);

			thirdPartyHistoDTO.setCategory("DAKHLI/GovernmentPayslip");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue((String) response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT DAKHLI Response For getGovernmentPayslip API = [{}] ### ",
					response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_dakhli.service.DakhliApiService#getMasdrDepositedSalary(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public ResponseEntity<?> getMasdrDepositedSalary(String customerId, String periodInMonths) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("DAKHLI_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_URL", apiUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_KEY", appKey);
		platformKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_PLATFORM_KEY", platformKey);
		organizationNumber = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_ORGANIZATION_NUMBER", organizationNumber);
		requestReason = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_REQUEST_REASON", requestReason);

		String url = apiUrl + "masdr/deposited-salary/" + customerId + "/" + periodInMonths;

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("PLATFORM-KEY", platformKey);
		headers.add("ORGANIZATION-NUMBER", organizationNumber);
		headers.add("PACKAGE-NAME", platformKey);
		headers.add("REQUEST-REASON", requestReason);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT DAKHLI Request For getMasdrDepositedSalary API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<?> response =
					restTemplate.exchange(url, HttpMethod.GET, requestEntity, String.class);

			thirdPartyHistoDTO.setCategory("DAKHLI/MasdrDepositedSalary");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue((String) response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT DAKHLI Response For getMasdrDepositedSalary API = [{}] ### ",
					response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_dakhli.service.DakhliApiService#getMasdrAkeed(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public ResponseEntity<?> getMasdrAkeed(String establishmentNumber, String customerId) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("DAKHLI_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_URL", apiUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_KEY", appKey);
		platformKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_PLATFORM_KEY", platformKey);
		organizationNumber = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_ORGANIZATION_NUMBER", organizationNumber);
		requestReason = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_REQUEST_REASON", requestReason);

		String url = apiUrl + "gosi/akeed/engagement-status/" + establishmentNumber + "/Relation/"
				+ customerId;

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("PLATFORM-KEY", platformKey);
		headers.add("ORGANIZATION-NUMBER", organizationNumber);
		headers.add("PACKAGE-NAME", platformKey);
		headers.add("REQUEST-REASON", requestReason);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT DAKHLI Request For getMasdrAkeed API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<?> response =
					restTemplate.exchange(url, HttpMethod.GET, requestEntity, String.class);

			thirdPartyHistoDTO.setCategory("DAKHLI/MasdrAkeed");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue((String) response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT DAKHLI Response For getMasdrAkeed API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_dakhli.service.DakhliApiService#getGovernmentContract(java.lang.String,
	 * java.lang.String, java.lang.String)
	 */
	@Override
	public ResponseEntity<?> getGovernmentContract(String idType, String idValue,
			String contractNumber) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("DAKHLI_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_URL", apiUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_APP_KEY", appKey);
		platformKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_PLATFORM_KEY", platformKey);
		organizationNumber = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_ORGANIZATION_NUMBER", organizationNumber);
		requestReason = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"DAKHLI_API_REQUEST_REQUEST_REASON", requestReason);

		UriComponentsBuilder url = UriComponentsBuilder
				.fromUriString(apiUrl + "goverment/contracts").queryParam("idType", idType)
				.queryParam("idValue", idValue).queryParam("contractNumber", contractNumber);

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("PLATFORM-KEY", platformKey);
		headers.add("ORGANIZATION-NUMBER", organizationNumber);
		headers.add("PACKAGE-NAME", platformKey);
		headers.add("REQUEST-REASON", requestReason);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT DAKHLI Request For getGovernmentContract API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<?> response = restTemplate.exchange(url.toUriString(), HttpMethod.GET,
					requestEntity, String.class);

			thirdPartyHistoDTO.setCategory("DAKHLI/GovernmentContract");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue((String) response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT DAKHLI Response For getGovernmentContract API = [{}] ### ",
					response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

}
