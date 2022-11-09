<template>
  <div class="detail-container">
    <div style="margin-bottom:10px;" class="tag-div">
      <el-tag>{{`厚度(mm)：${currentRow.thick}`}}</el-tag><el-tag style="margin-left:5px;">{{`材质：${currentRow.material}`}}</el-tag>
    </div>
    <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight">
      <el-table-column prop="index" label="序号" align="center" width="55" type="index" />
      <el-table-column
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
      >
        <template v-slot:header>
          <el-tooltip class="item" effect="light" :content="`双击编号可预览图纸`" placement="top">
            <div style="display: inline-block">
              <span>编号</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" label="规格" align="center" :show-overflow-tooltip="true"/>
      <el-table-column key="length" prop="length" label="长度(mm)" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="material" prop="material" label="材质" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="quantity" prop="quantity" label="数量" align="center"/>
      <el-table-column key="netWeight" prop="netWeight" label="单净重(kg)" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <span>{{toThousand(scope.row.netWeight,DP.COM_WT__KG)}}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column key="serialNumber" prop="serialNumber" label="图形" align="center" :show-overflow-tooltip="true" /> -->
    </common-table>
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
    <!-- pdf预览 -->
    <drawing-preview-fullscreen-dialog
      v-model="showDrawing"
      :bool-bim="drawingRow?.boolBim"
      :serial-number="drawingRow?.serialNumber"
      :productId="drawingRow?.productId"
      :productType="drawingRow?.productType"
    />
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import { partDetail } from '@/api/plan/technical-manage/machine-part'

import useDrawing from '@compos/use-drawing'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import usePagination from '@compos/use-pagination'

import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', productTypeField: 'MACHINE_PART' })
import { isNotBlank } from '@data-type/index'

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const list = ref([])
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight({ extraBox: '.tag-div', paginate: true })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

// 请求参数
const params = computed(() => {
  return {
    projectId: props.currentRow.projectId,
    monomerId: props.currentRow.monomerId,
    areaId: props.currentRow.areaId,
    thick: props.currentRow.thick,
    material: props.currentRow.material
  }
})

watch(
  () => params.value,
  (val) => {
    if (isNotBlank(val)) {
      fetchDetail()
    }
  },
  { deep: true, immediate: true }
)

async function fetchDetail() {
  list.value = []
  if (!props.currentRow.projectId) {
    return
  }
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await partDetail({ ...params.value, ...queryPage })
    list.value = content
    tableLoading.value = false
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取详情失败', error)
    tableLoading.value = false
  }
}

</script>
