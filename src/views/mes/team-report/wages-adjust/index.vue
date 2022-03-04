<template>
  <div style="display: flex">
    <summary-list
      style="width: 50%"
      ref="summaryListRef"
      @setInfo="setFInfo"
      @setDetailInfo="setDetailInfo"
      @refreshAuditNumber="refreshAuditNumber"
    />
    <div style="border-right: 1px solid #ededed; height: calc(100vh - 100px)"></div>
    <div class="app-container" v-if="false">
      <div class="my-code">点击左表操作查看明细</div>
    </div>
    <detail-list
      v-else
      ref="detailListRef"
      style="width: 49%"
      :fQuery="fQuery"
      :fInfo="fInfo"
      @setInfo="setDetailInfo"
      @clearInfo="clearFInfo"
    >
      <template #auditBox>
        <el-badge v-permission="commonPermission.audit" :value="auditNumberBadge" :hidden="!auditNumberBadge" style="margin-right: 5px">
          <common-button size="mini" type="primary" @click="auditVisible = true">审核记录</common-button>
        </el-badge>
      </template>
    </detail-list>
    <edit-drawer
      v-model:visible="editVisible"
      :productType="fQuery?.productType"
      :detailInfo="detailInfo"
      :productTypeText="productTypeText"
      @refresh="refreshAuditNumber"
    ></edit-drawer>
    <audit-drawer v-model:visible="auditVisible" @refresh="refreshAuditNumber"></audit-drawer>
  </div>
</template>

<script setup>
import { checkNumber } from '@/api/mes/team-report/wages-adjust'
import { computed, provide, ref, defineProps, inject } from 'vue'

import { inStaffWagesAdjustPM as permission } from '@/page-permission/mes'
import { mapGetters } from '@/store/lib'
import { componentTypeEnum, teamAttributeEnum } from '@enum-ms/mes'

import summaryList from './summary-list'
import detailList from './detail-list'
import editDrawer from './module/edit-drawer'
import auditDrawer from './module/audit-drawer'

const props = defineProps({
  organizationType: {
    type: Boolean,
    default: teamAttributeEnum.IN_STAFF.V
  }
})

const commonPermission = inject('permission', permission)
provide('permission', commonPermission)

const summaryListRef = ref()
const detailListRef = ref()

const fQuery = computed(() => {
  return summaryListRef?.value?.query
})
const { globalProjectId } = mapGetters('globalProjectId')
provide('projectId', globalProjectId)

provide('fQuery', fQuery)
provide('organizationType', props.organizationType)

const fInfo = ref()
const detailInfo = ref()
const editVisible = ref(false)
const auditVisible = ref(false)
const auditNumberBadge = ref(0)

function setFInfo(info) {
  fInfo.value = Object.assign({}, info)
  detailListRef.value?.toQuery()
}

function setDetailInfo(info) {
  detailInfo.value = Object.assign({}, info)
  editVisible.value = true
}

function clearFInfo() {
  fInfo.value = undefined
}

async function refreshAuditNumber() {
  auditNumberBadge.value = await checkNumber({
    projectId: globalProjectId.value,
    organizationType: props.organizationType
  })
}

const productTypeText = computed(() => {
  return componentTypeEnum.V[fQuery.value?.productType]?.SL
})
</script>
