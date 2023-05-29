<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="修订版本"
    :wrapper-closable="false"
    size="900px"
    custom-class="history-version"
  >
    <template #content>
       <el-descriptions class="margin-top" :column="2" border label-width="110">
        <el-descriptions-item label-class-name="fileName" label="文件名称" :span="2">
          <div style="word-break:break-all;">{{currentRow.fileName}}</div>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="attachmentDTO" label="文件" :span="2">
          <template v-if="currentRow.attachmentDTO">
            <div style="cursor: pointer; color: #409eff;word-break:break-all;" @dblclick="attachmentView(currentRow.attachmentDTO)">{{ currentRow.attachmentDTO.name }}</div>
          </template>
          <span v-else>-</span>
        </el-descriptions-item>
        <el-descriptions-item label-class-name="processType" label="文件类型">{{planProcessTypeEnum.VL[currentRow.processType]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="boolSingleProject" label="文件属性">{{processUseTypeEnum.VL[currentRow.boolSingleProject]}}</el-descriptions-item>
        <el-descriptions-item label-class-name="project" :label="currentRow.boolSingleProject?'所属项目':'关联项目'" :span="2">
          <el-tooltip placement="top">
            <template #content>
              <template v-if="isNotBlank(currentRow.projectList)">
                <div v-for="item in currentRow.projectList" :key="item.id">{{projectNameFormatter(item)}}</div>
              </template>
              <template v-else>-</template>
            </template>
            <div class="project-div">
              <template v-if="isNotBlank(currentRow.projectList)">
                <template v-if="currentRow.projectList.length===1">
                  <span v-for="item in currentRow.projectList" :key="item.id">{{projectNameFormatter(item)}}</span>
                </template>
                <template v-else>
                  <span v-for="item in currentRow.projectList" :key="item.id">【{{projectNameFormatter(item)}}】</span>
                </template>
              </template>
              <template v-else>-</template>
            </div>
          </el-tooltip>
        </el-descriptions-item>
         <el-descriptions-item label-class-name="userName" label="上传人">{{currentRow.userName}}</el-descriptions-item>
        <el-descriptions-item label-class-name="uploadTime" label="上传日期">{{currentRow.uploadTime?parseTime(currentRow.uploadTime,'{y}-{m}-{d} {h}:{i}:{s}'):'-'}}</el-descriptions-item>
        <el-descriptions-item label-class-name="remark" label="备注">
          <div style="word-break:break-all;">{{currentRow.remark}}</div>
        </el-descriptions-item>
      </el-descriptions>
      <el-divider><span class="title">历史修订版本</span></el-divider>
      <common-table
        ref="detailRef"
        border
        :data="currentRow.processFileRecordDTOList"
        :max-height="maxHeight-380"
        style="width: 100%"
        class="table-form"
        :dataFormat="dataFormat"
      >
        <el-table-column label="序号" type="index" align="center" width="50" />
        <el-table-column prop="fileVersion" label="版本" align="center" width="80" />
        <el-table-column key="attachmentDTO" prop="attachmentDTO" label="文件" align="left">
          <template v-slot="scope">
            <template v-if="scope.row.attachmentDTO">
              <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(scope.row.attachmentDTO)">{{ scope.row.attachmentDTO.name }}</div>
            </template>
            <span v-else>-</span>
          </template>
        </el-table-column>
        <el-table-column prop="userName" label="上传人" align="center" width="90" />
        <el-table-column prop="uploadTime" label="上传时间" align="center" width="140" />
        <el-table-column label="操作" align="center" width="80">
          <template v-slot="scope">
            <common-button size="small" class="el-icon-view" type="primary" @click="attachmentView(scope.row.attachmentDTO)"/>
          </template>
        </el-table-column>
      </common-table>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import useVisible from '@compos/use-visible'

import { isNotBlank } from '@data-type/index'
import { projectNameFormatter } from '@/utils/project'
import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'

import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const drawerRef = ref()
const { maxHeight } = useMaxHeight(
  {
    mainBox: ['.history-version', '.table-form'],
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: false,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const pdfShow = ref(false)
const currentId = ref()

const dataFormat = ref([
  ['uploadTime', ['parse-time', '{y}-{m}-{d} {h}:{i}:{s}']]
])

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-descriptions__label.el-descriptions__cell.is-bordered-label){
  width:110px;
}
.project-div{
  word-break: break-all;
  overflow: hidden;
  text-overflow: ellipsis;
  display: -webkit-box;
  line-clamp: 2;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
}
</style>
