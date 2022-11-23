<template>
  <common-drawer ref="drawerRef" title="批量调整工价" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleRight> </template>
    <template #content>
      <!-- <div class="head-container">
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
      </div> -->
      <common-table
        :data-format="dataFormat"
        row-key="rowId"
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <!-- <belonging-info-columns showProject showMonomer /> -->
        <el-table-column prop="serialNumber" align="center" show-overflow-tooltip label="编号">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="specification" align="center" show-overflow-tooltip label="规格">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="quantity" align="center" show-overflow-tooltip label="数量">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="weight" align="center" show-overflow-tooltip label="重量（kg）">
          <template #default="{ row }">
            <span>{{ row.weight }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="unit" align="center" show-overflow-tooltip label="核算单位">
          <template #default="{ row }">
            <span>{{ row.unit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="wages" align="center" show-overflow-tooltip label="定额工价">
          <template #default="{ row }">
            <span>{{ row.wages }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="price" label="修改后单价" width="200">
          <template #default="{ row }">
            <common-input-number v-model="row.price" placeholder="请输入单价" :precision="2" :controls="false" style="width: 100%">
            </common-input-number>
          </template>
        </el-table-column>
        <!-- <el-table-column prop="auditStatus" show-overflow-tooltip label="状态" width="100px" align="center">
          <template #default="{ row }">
            <el-tag :type="reviewStatusEnum.V[row.auditStatus].TAG">
              {{ reviewStatusEnum.VL[row.auditStatus] }}
            </el-tag>
          </template>
        </el-table-column> -->
        <!-- <el-table-column align="center" label="操作" width="100">
          <template #default="{ row }">
            <common-button size="mini" type="primary" @click="showDetail(row)">查看</common-button>
          </template>
        </el-table-column> -->
      </common-table>
      <div class="operate" style="display:flex;justify-content:flex-end;margin-top: 8px">
        <common-button size="small">取消</common-button>
        <common-button type="primary" size="small">保存</common-button>
      </div>
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
  <!-- <detail-drawer v-model:visible="detailVisible" :itemInfo="itemInfo" @success="handleAuditSuccess"></detail-drawer> -->
</template>

<script setup>
import { checkList } from '@/api/mes/team-report/wages-adjust'
import { defineProps, defineEmits, inject, ref, watch } from 'vue'

// import { reviewStatusEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
// import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
// import monomerSelect from '@/components-system/plan/monomer-select'
// import detailDrawer from './audit-detail-drawer'

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

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']]
])

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.operate'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)
const fQuery = inject('fQuery')
const organizationType = inject('organizationType')
const query = ref({})
// const detailVisible = ref(false)
// const itemInfo = ref()
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
    console.log('获取批量调整数据失败', error)
  } finally {
    tableLoading.value = false
  }
}

// function showDetail(row) {
//   itemInfo.value = row
//   detailVisible.value = true
// }

// function handleAuditSuccess() {
//   hasAudit.value = true
//   fetchList()
// }

function beforeClose() {
  if (hasAudit.value) {
    emit('refresh')
  }
}
</script>
