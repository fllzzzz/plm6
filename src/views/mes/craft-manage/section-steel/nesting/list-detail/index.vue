<template>
  <!-- 部件清单 -->
  <common-drawer :before-close="handleClose" size="80%" :title="`部件清单`" modal append-to-body v-model:visible="innerVisible">
    <template #titleAfter>
      <el-tag effect="plain" type="danger" class="filter-item" size="mini">
        <span>提示：缺少NC文件：{{ lossQuantity }}</span>
      </el-tag>
    </template>
    <template #content>
      <div class="content-container" style="margin-bottom: 8px">
        <common-radio-button v-model="productionLineTypeEnum" :options="artifactProductLineEnum.ENUM" type="enum" class="filter-item" />
      </div>
      <common-table v-loading="innerLoading" ref="tableDrawerRef" :data="assembleData" :max-height="400" style="width: 100%" row-key="id">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomer.name" prop="monomer" :show-overflow-tooltip="true" label="单体" align="center">
          <template #default="{ row }">
            <span>{{ row.monomer.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="area.name" prop="areaName" :show-overflow-tooltip="true" label="区域" align="center">
          <template #default="{ row }">
            <span>{{ row.area.name }}</span>
          </template>
        </el-table-column>
        <el-table-column key="artifactStr" prop="artifactStr" :show-overflow-tooltip="true" label="关联构件编号" min-width="120px" align="center">
          <template #default="{ row }">
            <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V"
              :name="row.classificationName"
              color="#fad400"
              :offset="15"
            />
            <span>{{ row.artifactStr }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="部件编号"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" align="center">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" align="center">
          <template #default="{ row }">
            <span>{{ row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" align="center">
          <template #default="{ row }">
            <span>{{ row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" width="60px">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" align="center">
          <template #default="{ row }">
            <span>{{ row.netWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="boolHaveNC1" prop="boolHaveNC1" :show-overflow-tooltip="true" label="套料文件" align="center" sortable> 
          <template #default="{ row }">
            <div v-if="row.boolHaveNC1"><i style="color: #5ded5d" class="el-icon-check" /></div>
            <div v-else>
              <!-- 这里accept设置只能单个导入文件 不能导入.zip  需要导入压缩包使用 :accept="`.zip,.nc1`"-->
              <upload-btn
                :data="{ monomerId: row.monomer.id, productId:row.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V? row.id:row.assembleDetailId,projectId: row.project.id, productType: componentTypeEnum.ASSEMBLE.V, dataType: technicalDataTypeEnum.NC_DRAWING.V }"
                :accept="`.nc1`"
                tip=".nc1"
                btn-type="primary"
                btn-size="mini"
                class="filter-item"
                @success="uploadSuccess"
              />
            </div>
          </template>
        </el-table-column>
      </common-table>
    </template>
    <!-- 分页 -->
    <pagination />
  </common-drawer>
</template>

<script  setup>
import useVisible from '@compos/use-visible'
import { getAssembleList } from '@/api/mes/craft-manage/section-steel/nesting'
import { artifactProductLineEnum, componentTypeEnum } from '@enum-ms/mes'
import { technicalDataTypeEnum } from '@enum-ms/plan'
import { ref, defineProps, defineEmits, watch } from 'vue'
import pagination from '@crud/Pagination'
import { update } from '@/api/plan/technical-data-manage/technical-achievement'
import uploadBtn from '@/views/plan/technical-data-manage/technical-achievement/components/drawing-upload-btn.vue'
import { objectMerge } from '@/utils/data-type/object'

const emit = defineEmits(['success'])
const productionLineTypeEnum = ref(artifactProductLineEnum.TRADITION.V)
// const innerVisible = ref(false)
const assembleData = ref([])
const lossQuantity = ref(0)
const innerLoading = ref(false)
const areaId = ref()
const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  assembleList: {
    type: Object,
    default: () => {},
  },
})

watch(
  () => productionLineTypeEnum.value,
  (val) => {
    if (val) {
      lossQuantity.value = 0
      initAssembleData()
    }
  }
)
watch(
  () => props.assembleList.id,
  (val) => {
    if (val) {
      lossQuantity.value = 0
      initAssembleData()
    }
  }
)

const { visible: innerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: initAssembleData })

function uploadSuccess() {
  initAssembleData()
}

async function initAssembleData() {
  try {
    const { content } = await getAssembleList({
      areaId: props.assembleList.id,
      productionLineTypeEnum: productionLineTypeEnum.value,
    })
    content.map(v => {
      v.artifactStr = v.artifactTypesettingDTOS.map(o=>o.serialNumber)?.join('，') || ''
      v.classificationName = v.artifactTypesettingDTOS.map(o=>o.classificationName)[0]
      if(v.boolHaveNC1 === false) {
        lossQuantity.value += v.quantity
      } else {
         lossQuantity.value = 0
      }
    })
    assembleData.value = content
  } catch (e) {
    console.log('获取部件清单失败', e)
  }
}

</script>

<style>
</style>
