import SvgIcon from '@/components/SvgIcon/index.vue'

// svg图标注册
import 'vite-plugin-svg-icons/register'

const useSvgIcon = (app) => {
  // 组件注册
  app.component(SvgIcon.name, SvgIcon)
}

export default useSvgIcon
