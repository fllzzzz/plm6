<template>
  <common-drawer ref="drawerRef" title="领料量详情" v-model="pickingDrawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <!-- <el-tag size="small" effect="plain">
        项目：<span>{{ pickingInfo.project?.serialNumber }}-{{ pickingInfo.project?.name }}</span>
      </el-tag> -->
      <el-input
        v-model.trim="monomerId"
        size="small"
        placeholder="输入单体搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="searchQuery"
      />
      <el-input
        v-model.trim="serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px; margin-left: 8px"
        class="filter-item"
        clearable
        @keyup.enter="searchQuery"
      />
      <common-button
        class="filter-item"
        size="mini"
        style="margin-left: 8px"
        type="success"
        icon="el-icon-search"
        @click.stop="searchQuery"
      >
        搜索
      </common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <div style="width: 300px">
        <print-table :api-key="apiKey" :params="{ ...queryParams }" size="mini" type="warning" class="filter-item" />
      </div>
    </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight - 100"
        row-key="rowId"
        style="width: 100%"
        show-summary
        :summary-method="getSummaries"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体">
          <template #default="{ row }">
            <span>{{ row.monomer?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="length" label="长度" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="netWeight" label="单重" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="totalNetWeight" label="总重" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="groups.name" label="领料人" align="center">
          <template #default="{ row }">
            <span>{{ row.workshop?.name }}>{{ row.groups?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" align="center" prop="completeTime" label="领料日期" width="120">
          <template #default="{ row }">
            <span>{{ parseTime(row.completeTime, '{y}-{m}-{d}') }}</span>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
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
import { getTask } from '@/api/mes/task-tracking/wip-statistics.js'
import { defineProps, defineEmits, ref, computed } from 'vue'
import { parseTime } from '@/utils/date'
import { tableSummary } from '@/utils/el-extra'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  pickingInfo: {
    type: Object
  }
})

const { visible: pickingDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

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
const queryParams = computed(() => {
  return {
    projectId: props.pickingInfo?.project?.id
  }
})

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'totalNetWeight']
  })
}

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const { content = [], totalElements } = await getTask({
      ...queryParams.value,
      ...queryPage
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取领料量详情失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
