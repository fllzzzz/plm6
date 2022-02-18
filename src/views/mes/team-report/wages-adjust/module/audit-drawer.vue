<template>
  <common-drawer ref="drawerRef" title="班组工价审核列表" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleRight> </template>
    <template #content>
      <div class="head-container">
        <common-radio-button
          v-model="query.status"
          :options="reviewStatusEnum.ENUM"
          type="enum"
          showOptionAll
          class="filter-item"
          @change="fetchList"
        />
        <monomer-select
          v-model="query.monomerId"
          clearable
          :default="false"
          :project-id="query?.projectId"
          class="filter-item"
          @change="fetchList"
        />
      </div>
      <common-table row-key="rowId" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer />
        <el-table-column prop="userName" show-overflow-tooltip label="编辑人">
          <template #default="{ row }">
            <span>{{ row.userName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="createTime" show-overflow-tooltip label="编辑日期" align="center">
          <template #default="{ row }">
            <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column prop="auditUserName" show-overflow-tooltip label="审核人">
          <template #default="{ row }">
            <span v-empty-text>{{ row.auditUserName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="auditTime" show-overflow-tooltip label="审核日期" align="center">
          <template #default="{ row }">
            <span v-parse-time="{ val: row.auditTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column prop="auditStatus" show-overflow-tooltip label="状态" width="100px" align="center">
          <template #default="{ row }">
            <el-tag :type="reviewStatusEnum.V[row.auditStatus].TAG">
              {{ reviewStatusEnum.VL[row.auditStatus] }}
            </el-tag>
          </template>
        </el-table-column>
        <el-table-column align="center" label="操作" width="100">
          <template #default="{ row }">
            <common-button size="mini" type="primary" @click="showDetail(row)">查看</common-button>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
  <detail-drawer v-model:visible="detailVisible" :itemInfo="itemInfo" @success="handleAuditSuccess"></detail-drawer>
</template>

<script setup>
import { checkList } from '@/api/mes/team-report/wages-adjust'
import { defineProps, defineEmits, inject, ref, watch } from 'vue'

import { reviewStatusEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import monomerSelect from '@/components-system/plan/monomer-select'
import detailDrawer from './audit-detail-drawer'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: beforeClose })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)
const fQuery = inject('fQuery')
const organizationType = inject('organizationType')
const query = ref({})
const detailVisible = ref(false)
const itemInfo = ref()
const hasAudit = ref(false)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      query.value = {
        projectId: fQuery.value?.projectId
      }
      hasAudit.value = false
      fetchList()
    }
  },
  { immediate: true }
)
const tableLoading = ref(false)
const list = ref([])

async function fetchList() {
  try {
    tableLoading.value = true
    const { content, totalElements } = await checkList({
      organizationType: organizationType,
      ...query.value
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取工价调整审核列表失败', error)
  } finally {
    tableLoading.value = false
  }
}

function showDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}

function handleAuditSuccess() {
  hasAudit.value = true
  fetchList()
}

function beforeClose() {
  if (hasAudit.value) {
    emit('refresh')
  }
}
</script>
