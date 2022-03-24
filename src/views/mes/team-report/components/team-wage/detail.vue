<template>
  <common-drawer ref="drawerRef" :title="title" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleAfter>
      <el-tag effect="plain" size="medium">
        {{ info?.factory?.name }}/{{ info?.workshop?.name }}/{{ info?.productionLine?.name }}/{{ info?.leaderName }}
      </el-tag>
      <el-tag type="success" effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="{ val: query.startDate, fmt: '{y}-{m}-{d}' }" /> ~
        <span v-parse-time="{ val: query.endDate, fmt: '{y}-{m}-{d}' }" />
      </el-tag>
      <el-tag type="warning" effect="plain" size="medium">
        <span>工资总额：</span>
        <span v-to-fixed="{ k: 'YUAN', val: info?.price }" />
        <span>元</span>
      </el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          :api-key="apiKey"
          :params="printParams"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="list"
        :data-format="dataFormat"
        :max-height="maxHeight"
        row-key="rowId"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showProject showMonomer showProcess />
        <productType-base-info-columns :productType="info?.productType" :unShowField="['color']" />
        <el-table-column prop="userName" :show-overflow-tooltip="true" label="报工人" align="center">
          <template #default="{ row }">
            <span>{{ row.userName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="showUnit" :show-overflow-tooltip="true" label="核算单位" align="center">
          <template #default="{ row }">
            <span>{{ row.showUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="checkMete" :show-overflow-tooltip="true" label="生产量" align="center">
          <template #default="{ row }">
            <span>{{ row.checkMete }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="wage" :show-overflow-tooltip="true" label="单价(元)" align="center">
          <template #default="{ row }">
            <span>{{ row.wage }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="price" :show-overflow-tooltip="true" label="工资(元)" align="center">
          <template #default="{ row }">
            <span>{{ row.price }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="date" :show-overflow-tooltip="true" label="生产日期" align="center">
          <template #default="{ row }">
            <span>{{ row.date }}</span>
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
import { detail } from '@/api/mes/team-report/in-staff/piecework-system'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'

import { deepClone } from '@data-type/index'
import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import usePagination from '@compos/use-pagination'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  title: {
    type: String
  },
  info: {
    type: Object,
    default: () => {}
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

const dataFormat = ref([
  ['wage', ['to-fixed-ck', 'YUAN']],
  ['price', ['to-fixed-ck', 'YUAN']]
])

const tableLoading = ref(false)
const list = ref([])
const query = inject('query')
const permission = inject('permission')

const printParams = computed(() => {
  return Object.assign(deepClone(query), {
    factoryId: props.info?.factory?.id,
    productionLineId: props.info?.productionLine?.id,
    workshopId: props.info?.workshop?.id,
    productType: props.info?.productType,
    teamId: props.info.teamId,
    processId: props.info.processId
  })
})

const apiKey = computed(() => {
  if (props.info?.productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.ASSEMBLE.V | componentTypeEnum.MACHINE_PART.V)) {
    return 'mesStructureTeamWageDetail'
  }
  if (props.info?.productType & componentTypeEnum.ENCLOSURE.V) {
    return 'mesEnclosureTeamWageDetail'
  }
  return undefined
})

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content, totalElements } = await detail({
      ...printParams.value,
      ...queryPage
    })
    _list = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      const _unitObj = useWageQuotaUnit({ wageQuotaType: v.wageQuotaType })
      v.showUnit = _unitObj.meteUnit
      v.checkMete = useWageQuotaMeteConvert({
        length: v.mate,
        weight: v.mate,
        surfaceArea: v.mate,
        wageQuotaType: v.wageQuotaType
      }).convertMete
      // v.checkMete = v.mate
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取详情列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
