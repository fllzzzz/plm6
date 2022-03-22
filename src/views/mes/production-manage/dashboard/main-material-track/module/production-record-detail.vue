<template>
  <common-drawer ref="drawerRef" title="构件生产记录" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="90%">
    <template #titleAfter>
      <el-tag effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="{ val: startDate, fmt: '{y}-{m}-{d}' }" />
        ~
        <span v-parse-time="{ val: endDate, fmt: '{y}-{m}-{d}' }" />
      </el-tag>
    </template>
    <template #titleRight> </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="list"
        :dataFormat="productFormat[componentTypeEnum.ARTIFACT.V]"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer />
        <productType-full-info-columns :productType="componentTypeEnum.ARTIFACT.V" :unShowField="['remark', 'surfaceArea']">
          <template #quantity>
            <el-table-column prop="quantity" label="数量" align="center" min-width="70px" />
          </template>
        </productType-full-info-columns>
        <el-table-column prop="createTime" :show-overflow-tooltip="true" label="生产日期" align="center" width="120px">
          <template #default="{ row }">
            <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
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
</template>

<script setup>
import { productionDetail } from '@/api/mes/production-manage/dashboard/main-material-track'
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import moment from 'moment'

import { componentTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import { productFormat } from '@/utils/columns-format/mes'
import productTypeFullInfoColumns from '@comp-mes/table-columns/productType-full-info-columns'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: Number
  },
  month: {
    type: String
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

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const startDate = computed(() => {
  return props.month && moment(moment(props.month)).startOf('month').valueOf()
})

const endDate = computed(() => {
  return props.month && moment(moment(props.month)).endOf('month').valueOf()
})

const tableLoading = ref(false)
const list = ref([])

async function fetchList() {
  try {
    tableLoading.value = true
    const _query = Object.assign({ projectId: props.projectId, month: props.month }, queryPage)
    const { content, totalElements } = await productionDetail(_query)
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取详情列表失败')
  } finally {
    tableLoading.value = false
  }
}
</script>
