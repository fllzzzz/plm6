<template>
  <common-drawer
    ref="drawerRef"
    :title="showType==='receive'?'入库明细':(showType==='send'?'出库明细':'库存明细')"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="delivery-detail"
    size="80%"
  >
    <template #titleAfter>
      <el-tag size="medium">{{`项目：${projectNameFormatter(detailInfo.project)}`}}</el-tag>
    </template>
    <template #titleRight>
      <!-- <print-table
        api-key="deliveryCargoList"
        :params="props.detailInfo?.id"
        size="mini"
        type="warning"
        class="filter-item"
      /> -->
    </template>
    <template #content>
      <el-input
        v-model.trim="serialNumber"
        size="small"
        placeholder="编号搜索"
        style="width: 200px;margin-bottom:10px;"
        class="filter-item"
        clearable
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click="fetchList" style="margin-left:10px;">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click="serialNumber=undefined;fetchList()">重置</common-button>
      <common-table :data="list" v-loading="tableLoading" :data-format="dataFormat" show-summary :summary-method="getSummaries" :max-height="maxHeight" v-if="visible">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" />
        <el-table-column key="areaName" prop="areaName" label="区域" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" />
        <el-table-column key="specification" prop="specification" label="规格" align="center" />
        <el-table-column key="length" prop="length" label="长度(mm)" align="center" />
        <el-table-column key="material" prop="material" label="材质" align="center" />
        <el-table-column key="intoQuantity" prop="intoQuantity" label="数量" align="center" v-if="showType==='receive'"/>
        <el-table-column key="outQuantity" prop="outQuantity" label="数量" align="center" v-if="showType==='send'"/>
        <el-table-column key="stockQuantity" prop="stockQuantity" label="库存数" align="center" v-if="showType==='storage'" />
        <el-table-column key="weight" prop="weight" label="重量（kg）" align="center" v-if="showType!=='detail'"/>
        <el-table-column key="createUserName" prop="createUserName" label="办理人" align="center" v-if="showType==='receive' || showType==='send'" />
        <el-table-column key="createTime" prop="createTime" label="入库日期" align="center" v-if="showType==='receive'" />
        <el-table-column key="createTime" prop="createTime" label="出库日期" align="center" v-if="showType==='send'" />
        <template v-if="showType==='detail'">
          <el-table-column label="入库" align="center">
            <el-table-column key="intoQuantity" prop="intoQuantity" label="入库数" align="center" />
            <el-table-column key="intoWeight" prop="intoWeight" label="重量（kg）" align="center" />
          </el-table-column>
          <el-table-column label="出库" align="center">
            <el-table-column key="outQuantity" prop="outQuantity" label="出库数" align="center" />
            <el-table-column key="outWeight" prop="outWeight" label="重量（kg）" align="center" />
          </el-table-column>
          <el-table-column label="库存" align="center">
            <el-table-column key="stockQuantity" prop="stockQuantity" label="库存数" align="center" />
            <el-table-column key="stockWeight" prop="stockWeight" label="重量（kg）" align="center" />
          </el-table-column>
        </template>
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
import { detail } from '@/api/intellect-workshop-manage/receive-send-storage'
import { ref, defineEmits, defineProps, watch } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'
import { projectNameFormatter } from '@/utils/project'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const emit = defineEmits(['update:modelValue', 'success'])
const serialNumber = ref()

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: undefined
  },
  detailQuery: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.delivery-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const dataFormat = ref([
  ['weight', ['to-fixed', DP.COM_WT__KG]],
  ['intoWeight', ['to-fixed', DP.COM_WT__KG]],
  ['outWeight', ['to-fixed', DP.COM_WT__KG]],
  ['stockWeight', ['to-fixed', DP.COM_WT__KG]],
  ['createTime', 'parse-time']
])

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['intoQuantity', 'outQuantity', 'stockQuantity', 'weight', 'intoWeight', 'outWeight', 'stockWeight']
  })
  return summary
}

// 获取明细
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await detail({ ...props.detailQuery, ...queryPage, serialNumber: serialNumber.value })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

</script>
