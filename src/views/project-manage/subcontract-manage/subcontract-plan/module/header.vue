<template>
  <div>
    <div v-show="crud.searchToggle" style="display:float;">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :default="false"
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-button size="small" type="primary" @click="accountVisible=true" style="float:right;" :disabled="!query.projectId" v-permission="crud.permission.account">分包登陆账号</common-button>
    </div>
    <subcontractAccount v-model="accountVisible" :query="crud.query"/>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import monomerSelect from '@/components-system/plan/monomer-select'
import subcontractAccount from './subcontract-account'

const defaultQuery = {
  monomerId: undefined
}

const { crud, query } = regHeader(defaultQuery)

const accountVisible = ref(false)

</script>
