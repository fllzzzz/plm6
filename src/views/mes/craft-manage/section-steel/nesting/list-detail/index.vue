<template>
  <!-- 部件清单 -->
  <common-drawer :before-close="handleClose" size="80%" :title="`部件清单`" modal append-to-body v-model:visible="innerVisible">
    <template #titleAfter>
      <el-tag effect="plain" type="danger" class="filter-item" size="mini">
        <span>提示：缺少NC1文件：{{ noFileData.summaryList }}</span>
      </el-tag>
    </template>
    <template #content>
      <div class="content-container" style="display: flex; margin-bottom: 8px">
        <common-radio-button
          v-model="productionLineTypeEnum"
          :options="hasIntelligent ? artifactProductLineEnum.ENUM : traditionLineEnum.ENUM"
          type="enum"
          style="align-self: center"
          class="filter-item"
          @change="handleProductionLineChange"
        />
        <common-select
          v-model="boolHaveNC1"
          :options="fileNC1TypeEnum.ENUM"
          type="enum"
          size="small"
          clearable
          class="filter-item"
          placeholder="是否导入NC1文件"
          style="width: 180px; margin-left: 8px"
          @change="handleFileStatusChange"
        />
      </div>
      <common-table v-loading="innerLoading" ref="tableDrawerRef" :data="assembleData" :max-height="maxHeight" style="width: 100%">
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
        <el-table-column
          key="artifactStr"
          prop="artifactStr"
          :show-overflow-tooltip="true"
          label="关联构件编号"
          min-width="120px"
          align="center"
        >
          <template #default="{ row }">
            <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.classificationName === '钢柱'"
              :name="row.classificationName"
              color="#fad400"
              :offset="15"
            />
            <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.classificationName === '钢梁'"
              :name="row.classificationName"
              color="#40ed8d"
              :offset="15"
            />
            <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.classificationName === '短梁'"
              :name="row.classificationName"
              color="#00babd"
              :offset="15"
            />
            <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.classificationName === '长短梁'"
              :name="row.classificationName"
              color="#ff7800"
              :offset="15"
            />
            <span>{{ row.artifactStr }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="部件编号" align="center">
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
        <el-table-column key="boolHaveNC1" prop="boolHaveNC1" :show-overflow-tooltip="true" label="套料文件" align="center">
          <template #default="{ row }">
            <div v-if="row.boolHaveNC1"><i style="color: #5ded5d" class="el-icon-check" /></div>
            <div v-else>
              <!-- 这里accept设置只能单个导入文件 不能导入.zip  需要导入压缩包使用 :accept="`.zip,.nc1`"-->
              <upload-btn
                :data="{
                  monomerId: row.monomer.id,
                  projectId: row.project.id,
                  productType: componentTypeEnum.ASSEMBLE.V,
                  dataType: technicalDataTypeEnum.NC_DRAWING.V,
                }"
                v-permission="permission.import"
                :accept="`.nc1`"
                tip=".nc1"
                :upload-fun="upload"
                :before-upload-hook="(file) => beforeUploadHook(file, row.serialNumber)"
                btn-type="primary"
                btn-name="文件导入"
                btn-size="mini"
                class="filter-item"
                @success="uploadSuccess"
              />
            </div>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
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
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { getAssembleList, getNoFileList } from '@/api/mes/craft-manage/section-steel/nesting'
import { artifactProductLineEnum, traditionLineEnum, componentTypeEnum, fileNC1TypeEnum } from '@enum-ms/mes'
import { mesExtrusionNestingPM as permission } from '@/page-permission/mes'
import { technicalDataTypeEnum } from '@enum-ms/plan'
import { ref, defineProps, defineEmits, watch, reactive } from 'vue'
import usePagination from '@compos/use-pagination'
import { upload } from '@/api/plan/technical-data-manage/technical-achievement'
import uploadBtn from '@/components/file-upload/SingleFileUploadBtn.vue'
import { ElMessage, ElNotification } from 'element-plus'
import { mapGetters } from '@/store/lib'

const emit = defineEmits(['success'])
const productionLineTypeEnum = ref(artifactProductLineEnum.TRADITION.V)
const boolHaveNC1 = ref()
const assembleData = ref([])
const noFileData = reactive({
  summaryList: 0
})

const { hasIntelligent } = mapGetters('hasIntelligent')
const innerLoading = ref(false)
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  assembleList: {
    type: Object,
    default: () => {}
  }
})

// watch(
//   () => productionLineTypeEnum.value,
//   (val) => {
//     initNoFileList()
//     initAssembleData()
//   },
//   { deep: true }
// )

watch(
  () => boolHaveNC1.value,
  (val) => {
    initNoFileList()
  },
  { deep: true }
)

const { visible: innerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: initAssembleData })

function showHook() {
  if (props.assembleList.id) {
    productionLineTypeEnum.value = artifactProductLineEnum.TRADITION.V
    boolHaveNC1.value = undefined
    initAssembleData()
    initNoFileList()
  }
}

function beforeUploadHook(file, matchSerialNumber) {
  const assembleNumber = file.name.substring(0, file.name.indexOf('.'))
  if (matchSerialNumber) {
    if (matchSerialNumber !== assembleNumber) {
      ElMessage.error(`上传文件编号需为${matchSerialNumber}`)
      return false
    }
  }
  return true
}

function uploadSuccess() {
  ElNotification({
    title: '上传成功',
    type: 'success',
    duration: 2500
  })
  initAssembleData()
  initNoFileList()
}

async function initAssembleData() {
  let _list = []
  innerLoading.value = true
  try {
    const { content = [], totalElements } = await getAssembleList({
      areaId: props.assembleList.id,
      productionLineTypeEnum: productionLineTypeEnum.value,
      boolHaveNC1: boolHaveNC1.value,
      ...queryPage
    })
    _list = content.map((v) => {
      v.artifactStr = v.artifactTypesettingDTO?.serialNumber
      v.classificationName = v.artifactTypesettingDTO?.classificationName
      return v
    })
    setTotalPage(totalElements)
  } catch (e) {
    console.log('获取部件清单失败', e)
  } finally {
    assembleData.value = _list
    innerLoading.value = false
  }
}
async function initNoFileList() {
  try {
    const data = await getNoFileList({
      areaId: props.assembleList.id,
      productionLineTypeEnum: productionLineTypeEnum.value
    })
    noFileData.summaryList = data
  } catch (e) {
    console.log('获取缺失nc1文件数量失败', e)
  }
}

function handleFileStatusChange() {
  initAssembleData()
}

function handleProductionLineChange() {
  initAssembleData()
  initNoFileList()
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>

<style></style>
