<template>
  <common-drawer ref="drawerRef" title="质检报表详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleRight> </template>
    <template #content>
      <div class="head-container">
        <el-input
          v-model="detailQuery.projectName"
          placeholder="输入项目搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="fetchList"
        />
        <el-input
          v-model="detailQuery.serialNumber"
          placeholder="输入编号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="fetchList"
        />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </div>
      <common-table v-loading="tableLoading" :data="list" :max-height="maxHeight - 155" row-key="rowId" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject />
        <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" width="120px">
          <template #default="{ row }">
            <span>{{ row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" width="120px">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="processName" label="检验工序" width="120px" align="center">
          <template #default="{ row }">
            <el-tag type="warning" effect="plain">{{ row.processName }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="inspectionQuantity" label="检验数">
          <template #default="{ row }">
            <span>{{ row.inspectionQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="qhseQuantity" label="不合格数">
          <template #default="{ row }">
            <span class="tc-danger">{{ row.qhseQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="date" label="检验日期" width="120">
          <template #default="{ row }">
            <span>{{ row.date }}</span>
          </template>
        </el-table-column>
        <!-- <productType-base-info-columns :productType="query.productType" /> -->
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
</template>

<script setup>
import { detail } from '@/api/bridge/QHSE-manage/quality-inspection-report'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
// import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object
  },
  projectId: {
    type: Number
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
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

const tableLoading = ref(false)
const list = ref([])
const detailQuery = ref({})
const query = inject('query')

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    } else {
      detailQuery.value = {}
    }
  },
  { immediate: true }
)

function resetQuery() {
  detailQuery.value = {}
  fetchList()
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { endDate, productType, startDate } = query
    const { content, totalElements } = await detail({
      endDate,
      productType,
      startDate,
      userId: props.info?.userId,
      projectId: props.projectId,
      ...queryPage,
      ...detailQuery.value
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取质检报表详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
