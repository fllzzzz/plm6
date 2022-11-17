<template>
  <common-drawer
    ref="drawerRef"
    customClass="cutting-detail"
    :title="`${props.cuttingDetailData?.workshop?.name}>${props.cuttingDetailData?.productionLine?.name}切割详情`"
    v-model="cuttingDrawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="63%"
  >
    <template #titleAfter>
      <common-radio-button
        v-model="orderType"
        :options="typeEnum.ENUM"
        type="enum"
        size="mini"
        class="filter-item"
        @change="handleChange"
      />
    </template>
    <template #titleRight>
      <common-button size="mini" type="success" @click="printInf">打印</common-button>
    </template>
    <template #content>
      <div :style="`height:${maxHeight}px`" v-if="orderType === typeEnum.NESTING_TASK_ORDER.V">
        <pdf :url="source" :type="'canvas'" :pdfjsDistPath="pdfjsDistPath" />
      </div>
      <!--表格渲染-->
      <div v-if="orderType === typeEnum.SORTING_ORDER.V">
        <common-table ref="table" :data="cuttingData" empty-text="暂无数据" :max-height="maxHeight" style="width: 100%">
          <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column :show-overflow-tooltip="true" prop="picturePath" key="picturePath" label="图形" align="center">
            <template v-slot="scope">
              <el-image style="width: 100%" :src="scope.row.picturePath" fit="cover" />
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.serialNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column
            :show-overflow-tooltip="true"
            prop="workshopLine"
            :key="item"
            v-for="item in workshopList"
            :label="`${item.workShopName}>${item.productionLineName}`"
            align="center"
            min-width="110px"
          >
            <template v-slot="scope">
              <span>{{ scope.row[`quantity${item.productionLineId}`] || '0' }}</span>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
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
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import { sortingListEnum as typeEnum } from '@enum-ms/mes'
import { defineProps, defineEmits, ref } from 'vue'
import { showCuttingPdf, showInfo } from '@/api/mes/work-order-manage/machine-part.js'
import { printPDF } from '@/utils/print/pdf-print'
import pdf from '@/components/PDF/pdf'

const pdfjsDistPath = import.meta.env.BASE_URL + 'assets'
const emit = defineEmits(['update:visible'])
const drawerRef = ref()
const cuttingData = ref([]) // 切割分拣单详情数据

const source = ref('')
const sourceBase64 = ref('')
const orderType = ref(typeEnum.NESTING_TASK_ORDER.V)
const workshopList = ref([])

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  cuttingDetailData: {
    type: Object,
    default: () => {}
  },
  processType: {
    type: Number
  }
})

const { maxHeight } = useMaxHeight({
  mainBox: ['.cutting-detail'],
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  extraHeight: 100,
  clientHRepMainH: true,
  paginate: true
})

const { visible: cuttingDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: nestingDetailGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: cuttingDetailGet })

// 切割 套料任务单
async function nestingDetailGet() {
  orderType.value = typeEnum.NESTING_TASK_ORDER.V
  try {
    const data = await showCuttingPdf({ cutId: props.cuttingDetailData.id })
    source.value = await getUrlByFileReader(data)
    sourceBase64.value = await getBase64ByFileReader(data)
  } catch (error) {
    console.log('获取套料任务单失败', error)
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

// 文件流转换为base64
function getBase64ByFileReader(res) {
  return new Promise((resolve, reject) => {
    if (res && res.data && res.data.size) {
      const dataInfo = res.data
      const reader = new window.FileReader()
      reader.readAsDataURL(dataInfo)

      reader.onload = function (e) {
        const result = e.target.result.split('base64,')[1]
        resolve(result)
      }
    } else {
      reject()
    }
  })
}
function printInf() {
  printPDF({ pdfData: sourceBase64.value })
}
// 切割 分拣单
async function cuttingDetailGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await showInfo({
      cutId: props.cuttingDetailData.id,
      processType: props.processType,
      ...queryPage
    })
    setTotalPage(totalElements)
    content.map((v) => {
      v.list.map((m) => {
        if (workshopList.value.findIndex((k) => k.productionLineId === m.productionLineId) < 0) {
          workshopList.value.push({
            productionLineId: m.productionLineId,
            productionLineName: m.productionLineName,
            workShopId: m.workShopId,
            workShopName: m.workShopName
          })
        }
      })
      v.list.map((m) => {
        workshopList.value.map((k) => {
          if (m.productionLineId === k.productionLineId) {
            v[`quantity${k.productionLineId}`] = m.quantity
          }
        })
      })
    })
    _list = content
  } catch (error) {
    console.log('获取切割分拣单失败', error)
  } finally {
    cuttingData.value = _list
  }
}

function handleChange(val) {
  if (val === typeEnum.SORTING_ORDER.V) {
    cuttingDetailGet()
  } else {
    nestingDetailGet()
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.pdfViewer .page) {
  margin: 1px;
}
</style>

