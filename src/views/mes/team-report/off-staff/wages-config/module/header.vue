<template>
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
    <process-radio-button
      ref="processRef"
      v-model="query.processId"
      :productType="query.productType"
      class="filter-item"
      @change="handleProcess"
    />
    <div>
      <el-input
        v-model.trim="query.leaderName"
        placeholder="输入班组搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.monomerName"
        placeholder="输入单体搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.areaName"
        placeholder="输入区域搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="输入编号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
  <crudOperation>
    <template #optLeft>
      <common-button v-show="modifying" type="warning" size="mini" @click.stop="handelModifying(false, true)">取消编辑</common-button>
      <common-button v-show="modifying" type="success" size="mini" @click.stop="previewVisible = true">预览并保存</common-button>
      <common-button
        v-permission="crud.permission?.edit"
        v-show="!modifying"
        type="primary"
        size="mini"
        style="margin-left:0px;"
        @click.stop="handelModifying(true)"
      >
        编辑
      </common-button>
    </template>
    <template #viewLeft>
      <el-badge v-permission="crud.permission?.audit" :value="auditNumberBadge" :hidden="auditNumberBadge === 0">
        <common-button size="mini" type="primary" @click="auditVisible = true">审核</common-button>
      </el-badge>
    </template>
  </crudOperation>
  <audit-drawer v-model:visible="auditVisible" @refresh="crud.toQuery()"></audit-drawer>
  <preview
    v-model:visible="previewVisible"
    :list="crud.data"
    @refresh="
      crud.toQuery();
      handelModifying(false, true)
    "
  ></preview>
</template>

<script setup>
import { checkNumber } from '@/api/mes/team-report/off-staff'
import { ref, defineProps, defineEmits, nextTick } from 'vue'
import { processMaterialListTypeEnum as typeEnum, artifactProcessEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import processRadioButton from '@comp-mes/process-radio-button'
import auditDrawer from './audit-drawer'
import preview from './preview'

const defaultQuery = {
  productType: artifactProcessEnum.ONCE.V,
  componentType: typeEnum.ARTIFACT.V,
  processId: undefined
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
useGlobalProjectIdChangeToQuery(crud)

defineProps({
  modifying: {
    type: Boolean,
    default: false
  },
  wageQuotaType: {
    type: Number
  }
})

const emit = defineEmits(['update:modifying', 'update:wageQuotaType'])

const auditVisible = ref(false)
const previewVisible = ref(false)

function handelModifying(modifying, reset = false) {
  // 取消分配，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.unitPrice = v.originPrice // 未分配数量还原
      return v
    })
  }
  emit('update:modifying', modifying)
}

const processRef = ref()
const processInfo = ref()
function handleProcess() {
  nextTick(() => {
    processInfo.value = processRef.value.getOptionInfo(query.processId)
    emit('update:wageQuotaType', processInfo.value?.wageQuotaType)
  })
  crud.toQuery()
}

// 获取审核数量
const auditNumberBadge = ref(0)
CRUD.HOOK.beforeToQuery = () => {
  getAuditNumber()
}
async function getAuditNumber() {
  auditNumberBadge.value = await checkNumber(query)
}
</script>
