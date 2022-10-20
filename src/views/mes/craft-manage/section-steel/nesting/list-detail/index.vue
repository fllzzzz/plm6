<template>
  <!-- 部件清单 -->
  <common-drawer :before-close="handleClose" size="60%" :title="`部件清单`" modal append-to-body v-model:visible="innerVisible">
    <template #content>
      <div class="content-container" style="margin-bottom: 10px">
        <el-tag effect="plain" type="danger" class="filter-item" size="medium">
          <span style="cursor: pointer;" @click.stop="missFile(props.partData)">提示：缺少NC文件：2</span>
        </el-tag>
      </div>
      <common-table v-loading="innerLoading" ref="tableDrawerRef" :data="props.partData" :max-height="400" style="width: 100%" row-key="id">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomerId" prop="monomerId" :show-overflow-tooltip="true" label="单体" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="artifactNumber"
          prop="artifactNumber"
          :show-overflow-tooltip="true"
          label="关联构件编号"
          align="center"
        >
        <template #default="{ row }">
            <span>{{ row.artifactNumber }}</span>
        </template>
        </el-table-column>
        <el-table-column
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="部件编号"
          min-width="60"
          align="center"
        >
        <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
        </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.netWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="nestingFile" prop="nestingFile" :show-overflow-tooltip="true" label="套料文件" min-width="60" align="center">
          <template #default="{ row }">
            <div v-if="row.nestingFile"><i style="color: #5ded5d" class="el-icon-check" /></div>
            <div v-else>
              <upload-btn
                :data="{ areaId: areaId, importType: 1 }"
                :upload-fun="listUpload"
                btn-name="导入文件"
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
// import useMaxHeight from '@compos/use-max-height'
import { ref, defineProps, defineEmits } from 'vue'
import pagination from '@crud/Pagination'
import { listUpload } from '@/api/plan/technical-manage/artifact-tree'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'

const emit = defineEmits(['success'])
// const innerVisible = ref(false)
const innerLoading = ref(false)
const areaId = ref()
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  partData: {
    type: Array,
    default: () => []
  }
})
const { visible: innerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
function uploadSuccess() {
  emit('success')
}

// nc文件缺失，点击置顶
function missFile(data) {
  data.forEach((v, index) => {
    if (v.mete > 654) {
      data.splice(index, 1)
      data.unshift(v)
    }
  })
  uploadSuccess()
}

</script>

<style>
</style>
