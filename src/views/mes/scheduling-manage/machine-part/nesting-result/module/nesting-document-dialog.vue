<template>
  <common-dialog
    title="套料文档详情"
    customClass="nesting-document-dialog"
    v-model="nestingDialogVisible"
    :close-on-click-modal="false"
    top="5vh"
    width="1200px"
    :showClose="true"
    :before-close="handleClose"
  >
    <el-descriptions :column="2" :data="nestingDocumentList" border class="nesting-document">
      <el-descriptions-item align="center" label="项目" span="2">
        {{ nestingDocumentList.projectName }} ( {{ nestingDocumentList.monomerList }} )
      </el-descriptions-item>
      <el-descriptions-item align="center" label="任务单号">{{ nestingDocumentList.orderNumber }}</el-descriptions-item>
      <el-descriptions-item align="center" label="套料日期">{{
        parseTime(nestingDocumentList.createTime, '{y}-{m}-{d}')
      }}</el-descriptions-item>
      <el-descriptions-item align="center" label="零件数量（件/kg）">
        {{ nestingDocumentList.quantity }} / {{ nestingDocumentList.totalNetWeight }}
      </el-descriptions-item>
      <el-descriptions-item align="center" label="操作">
        <common-button
          class="filter-item"
          size="mini"
          type="primary"
          v-if="nestingList.issueStatusEnum !== issueStatusEnum.IN_NESTING.V"
          @click="download"
        >
          下载
        </common-button>
      </el-descriptions-item>
    </el-descriptions>
    <!--任务单-->
    <div
      v-loading="taskLoading"
      v-if="nestingList.issueStatusEnum !== issueStatusEnum.IN_NESTING.V"
      :style="`height:${maxHeight}px; overflow: auto`"
    >
      <pdf :url="taskOrderPDF" :type="'canvas'" :pdfjsDistPath="pdfjsDistPath" />
    </div>
  </common-dialog>
</template>

<script setup>
// import { saveNestingTask } from '@/api/mes/scheduling-manage/common'
import { getProjectDetail, getShowPdf } from '@/api/mes/scheduling-manage/machine-part'
import { machinePartSchedulingIssueStatusEnum as issueStatusEnum } from '@enum-ms/mes'
import { defineEmits, defineProps, ref } from 'vue'
import { parseTime } from '@/utils/date'
import { ElNotification } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import pdf from '@/components/PDF/pdf'

const pdfjsDistPath = import.meta.env.BASE_URL + 'assets'
const nestingDocumentList = ref([])
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  nestingList: {
    type: Object,
    default: () => {}
  }
})
const taskLoading = ref(false)
const taskOrderPDF = ref('')

const { visible: nestingDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.nesting-document-dialog',
    extraBox: ['.el-dialog__header', '.nesting-document'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  nestingDialogVisible
)

function showHook() {
  fetchDocument()
  nestingDetailGet()
}

async function fetchDocument() {
  try {
    const data = await getProjectDetail({ id: props.nestingList.id })
    data.projectName = data?.projectInfoList?.map((v) => v.name).join('')
    data?.projectInfoList?.forEach((v) => {
      data.monomerList = v.monomers.map((o) => o.name).join(', ')
      return v
    })
    console.log(data, 'data')
    nestingDocumentList.value = data || []
  } catch (err) {
    console.log('获取当前的项目下的信息失败', err)
  }
}

// 套料文档
async function nestingDetailGet() {
  try {
    taskLoading.value = true
    taskOrderPDF.value = ''
    const data = await getShowPdf({ id: props.nestingList.id })
    taskOrderPDF.value = await getUrlByFileReader(data)
  } catch (error) {
    console.log('获取套料任务单失败', error)
  } finally {
    taskLoading.value = false
  }
}

// 转化为文件流
function getUrlByFileReader(res) {
  return new Promise((resolve, reject) => {
    if (res && res.data && res.data.size) {
      const dataInfo = res.data
      const reader = new window.FileReader()
      // 使用readAsArrayBuffer读取文件, result属性中将包含一个 ArrayBuffer 对象以表示所读取文件的数据
      reader.readAsArrayBuffer(dataInfo)
      reader.onload = function (e) {
        console.log(e, dataInfo, 'rrrr')
        const result = e.target.result
        const contentType = dataInfo.type
        // 生成blob图片,需要参数(字节数组, 文件类型)
        const blob = new Blob([result], { type: contentType })
        // 使用 Blob 创建一个指向类型化数组的URL, URL.createObjectURL是new Blob文件的方法,可以生成一个普通的url,可以直接使用,比如用在img.src上
        const url = window.URL.createObjectURL(blob)
        console.log(url) // 产生一个类似 blob:d3958f5c-0777-0845-9dcf-2cb28783acaf 这样的URL字符串
        resolve(url)
      }
    } else {
      reject()
    }
  })
}
// 下载钢板图形
function download() {
  if (props.nestingList.dwgReportUrl !== null) {
    window.location.href = props.nestingList.dwgReportUrl
  } else {
    ElNotification({ title: '下载失败', message: '暂无钢板图形 ', type: 'error' })
  }
}
</script>

<style scoped>
.tip {
  display: inline-block;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>
