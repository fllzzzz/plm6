<template>
 <common-drawer @close="handleClose" v-model="showDrawer" size="90%" :title="props.inventoryType === 1 ? '期初库存量' : props.inventoryType === 2 ? '入库量' : props.inventoryType === 3 ? '出库量' : '期末库存量'">
  <template #titleRight>
    <print-table
    :api-key="props.inventoryType === 1 ? 'enclosureTotalBeginningInventoryDetail' : props.inventoryType === 2 ? 'enclosureTotalInboundInventoryDetail' : props.inventoryType === 3 ? 'enclosureTotalOutboundInventoryDetail' : 'enclosureTotalEndInventoryDetail'"
    v-permission="permission.detailPrint"
    :params="{
      ...queryData,
      ...uploadData
    }"
    ></print-table>
  </template>
  <template #content>
    <div class="head-container">
      <div class="header-div">
        <project-cascader class="filter-item" v-model="uploadData.projectId" @change="selectProject" clearable />
        <common-select
            v-model="uploadData.enclosurePlanId"
            :options="areaInfo"
            type="other"
            :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
            size="small"
            clearable
            placeholder="请选择批次"
            class="filter-item"
            style="width: 200px; margin-left: 3px"
            @change="fetchList"
          />
          <el-input
            v-model.trim="uploadData.serialNumber"
            size="small"
            placeholder="编号搜索"
            style="width: 200px; margin-bottom: 10px; margin-left: 3px"
            class="filter-item"
            clearable
            @keyup.enter="fetchList"
          />
          <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click="fetchList" style="margin-left: 10px">
            搜索
          </common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click=";(uploadData = {}), fetchList()">
            重置
          </common-button>
      </div>
        <common-table :data="list" v-loading="tableLoading">
          <el-table-column label="序号" align="center" type="index" min-width="50"></el-table-column>
          <el-table-column label="项目" align="center" min-width="180" prop="project">
            <template #default="{ row }">
              <span>{{ row.project?.serialNumber || '' }}-{{ row.project?.shortName || '' }}</span>
            </template>
          </el-table-column>
          <el-table-column label="名称" align="center" prop="name" min-width="180"></el-table-column>
          <el-table-column label="编号" align="center" prop="serialNumber" min-width="120"></el-table-column>
          <el-table-column label="版型" align="center" prop="plate" min-width="100"></el-table-column>
          <el-table-column label="单长" align="center" prop="length"></el-table-column>
          <el-table-column align="center" prop="quantity">
            <template #header>
              <span v-if="props.inventoryType === 1">期初数</span>
              <span v-else-if="props.inventoryType === 2">入库数</span>
              <span v-else-if="props.inventoryType === 3">出库数</span>
              <span v-else>期末数</span>
            </template>
          </el-table-column>
          <el-table-column align="center" prop="totalLength">
            <template #header>
              <span v-if="props.inventoryType === 1">期初量</span>
              <span v-else-if="props.inventoryType === 2">入库量</span>
              <span v-else-if="props.inventoryType === 3">出库量</span>
              <span v-else>期末量</span>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
        layout="total, prev, pager, next, sizes"
        style="margin-top: 8px"
      />
    </div>
  </template>
 </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, computed, ref } from 'vue'
import useVisible from '@compos/use-visible'
import { enclosureProductDetail } from '@/api/ship-manage/pack-and-ship/enclosure-product-receive-send-storage'
import { mesProductSendReceiveStoragePM as permission } from '@/page-permission/ship-manage'
import { getEnclosureBatch } from '@/api/mes/common.js'
import usePagination from '@compos/use-pagination'
import projectCascader from '@comp-base/project-cascader'

const props = defineProps({
  showDetailDrawer: {
    type: Boolean
  },
  query: {
    type: Object,
    default: () => {}
  },
  inventoryType: {
    type: Number
  }
})

const list = ref()
const tableLoading = ref(false)
const areaInfo = ref([])
const uploadData = ref({
  enclosurePlanId: undefined,
  serialNumber: undefined,
  projectId: undefined
})

const queryData = computed(() => {
  return {
    projectId: props.query.projectId,
    category: props.query.category,
    dateTime: props.query.dateTime,
    productType: props.query.productType,
    type: props.inventoryType
  }
})

const emit = defineEmits(['update:showDetailDrawer'])

const { visible: showDrawer, handleClose } = useVisible({
  emit,
  props,
  field: 'showDetailDrawer',
  showHook: fetchList
})

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

async function fetchBatch() {
  try {
    const data = await getEnclosureBatch(uploadData.value?.projectId)
    areaInfo.value = data || []
    areaInfo.value = areaInfo.value.filter(v => v.category === props.category)
  } catch (e) {
    console.log('获取围护的批次失败', e)
  }
}

async function fetchList() {
  list.value = []
  try {
    tableLoading.value = true
    const res = await enclosureProductDetail({
      ...queryData.value,
      ...uploadData.value,
      ...queryPage
    })
    console.log(res)
    console.log(props.query)
    list.value = res.content
    setTotalPage(res.totalElements)
  } catch (error) {
    console.log(error)
  } finally {
    tableLoading.value = false
  }
}

const selectProject = () => {
  fetchBatch()
  fetchList()
}

</script>
