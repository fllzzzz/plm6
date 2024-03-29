<template>
  <common-drawer
    ref="drawerRef"
    :title="`入发存${reportTypeEnum.VL[reportType]}报表${isSummary ? '' : '(' + itemInfo.shortName + ')'}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
    custom-class="mes-report-detail"
  >
    <template #titleRight> </template>
    <template #content>
      <!-- 表格渲染 -->
      <div v-loading="contentLoading">
        <component :is="currentTableView" :table-data="tableData" :max-height="maxHeight" :is-summary="isSummary" />
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
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getEnclosureDetail, getArtifactDetail } from '@/api/mes/manufactures-manage/report'
import { defineProps, defineEmits, ref, inject, computed } from 'vue'

import { reportComponentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import structureComponent from './structure'
import enclosureComponent from './enclosure'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  itemInfo: {
    type: Object,
    default: () => {}
  },
  reportType: {
    type: Number,
    default: undefined
  },
  isSummary: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.mes-report-detail',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    minHeight: 400,
    paginate: true,
    clientHRepMainH: true
  },
  drawerRef
)

const reportTypeEnum = inject('reportTypeEnum')

const productType = computed(() => {
  return parseInt(props.itemInfo.productType) || query.productType
})
const projectId = computed(() => {
  return props.itemInfo.id || query.projectId
})
const currentTableView = computed(() => {
  switch (productType.value) {
    case reportComponentTypeEnum.ARTIFACT.V:
      return structureComponent
    case reportComponentTypeEnum.ENCLOSURE.V:
      return enclosureComponent
    default:
      return ''
  }
})

// 获取数据
const query = inject('query')
const contentLoading = ref(false)
const tableData = ref([])
async function fetchList() {
  try {
    contentLoading.value = true
    const { startDate, endDate } = query
    const _query = Object.assign({ startDate, endDate, projectId: projectId.value, type: props.reportType }, queryPage)
    let _data = []
    switch (productType.value) {
      case reportComponentTypeEnum.ARTIFACT.V:
        _data = await getArtifactDetail(_query)
        break
      case reportComponentTypeEnum.ENCLOSURE.V:
        _data = await getEnclosureDetail(_query)
        break
      default:
        break
    }
    setTotalPage(_data.totalElements)
    tableData.value = _data.content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
  } catch (error) {
    console.log('入发存详情', error)
  } finally {
    contentLoading.value = false
  }
}
</script>
