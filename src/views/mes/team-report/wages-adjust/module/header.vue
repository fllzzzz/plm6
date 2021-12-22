<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <common-radio-button
          v-model="query.componentType"
          :options="typeEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="handleComponentType"
        />
        <common-radio-button
          v-if="query.componentType === typeEnum.ARTIFACT.V"
          v-model="query.productType"
          :options="artifactProcessEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        />
        <!-- <common-radio-button
          v-if="query.componentType === typeEnum.ENCLOSURE.V"
          v-model="query.category"
          :options="mesEnclosureTypeEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        /> -->
        <monomer-select v-model="query.monomerId" clearable :project-id="projectId" class="filter-item" />
        <rrOperation />
      </div>
    </template>
    <template #viewLeft>
      <el-badge v-if="auditNumberBadge" :value="auditNumberBadge">
        <common-button size="mini" type="primary" @click="auditVisible = true">审核</common-button>
      </el-badge>
    </template>
  </crudOperation>
  <audit-drawer v-model:visible="auditVisible" @refresh="crud.toQuery"></audit-drawer>
</template>

<script setup>
import { checkNumber } from '@/api/mes/team-report/wages-adjust'
import { inject, ref, watch } from 'vue'
import { processMaterialListTypeEnum as typeEnum, artifactProcessEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import auditDrawer from './audit-drawer'

const projectId = inject('projectId')
const auditVisible = ref(false)
const auditNumberBadge = ref(0)

const defaultQuery = {
  productType: artifactProcessEnum.ONCE.V,
  componentType: typeEnum.ARTIFACT.V,
  // category: mesEnclosureTypeEnum.PRESSED_PLATE.V,
  monomerId: undefined
}

function handleComponentType(val) {
  if (val === typeEnum.ARTIFACT.V) {
    query.productType = artifactProcessEnum.ONCE.V
  } else {
    query.productType = val
  }
  crud.toQuery()
}

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.beforeToQuery = () => {
  crud.query.projectId = projectId
}

CRUD.HOOK.beforeRefresh = () => {
  getAuditNumber()
}

watch(
  () => query.monomerId,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

async function getAuditNumber() {
  auditNumberBadge.value = await checkNumber(query)
}
</script>
