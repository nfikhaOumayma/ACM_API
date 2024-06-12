package com.acm.client;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;

/**
 * The Interface ExpensesClient.
 */
@FeignClient(value = "expenses-service", configuration = ClientConfiguration.class,
		decode404 = true)
@RibbonClient(name = "expenses-service", configuration = LoadbalancerRuleFeignConfiguration.class)
public interface ExpensesClient {

	/**
	 * Update document name.
	 *
	 * @param documentLabel the document label
	 * @param documentId the document id
	 */
	@PutMapping("/expenses-type/update-document-name/{documentID}")
	void updateDocumentName(@RequestBody String documentLabel,
			@PathVariable("documentID") Long documentId);

}
